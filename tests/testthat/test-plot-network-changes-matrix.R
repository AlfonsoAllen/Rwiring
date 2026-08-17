# =============================================================================
# Tests for plot_network_changes_matrix()
# =============================================================================


get_interaction_tile_layer <- function(p) {
  
  tile_indices <- which(vapply(
    p$layers,
    function(layer) {
      inherits(layer$geom, "GeomTile")
    },
    logical(1)
  ))
  
  interaction_indices <- tile_indices[
    vapply(
      p$layers[tile_indices],
      function(layer) {
        is.data.frame(layer$data) &&
          "Direction" %in% names(layer$data)
      },
      logical(1)
    )
  ]
  
  interaction_indices[1L]
}


get_impossible_tile_layer <- function(p) {
  
  tile_indices <- which(vapply(
    p$layers,
    function(layer) {
      inherits(layer$geom, "GeomTile")
    },
    logical(1)
  ))
  
  impossible_indices <- tile_indices[
    vapply(
      p$layers[tile_indices],
      function(layer) {
        is.data.frame(layer$data) &&
          all(
            c(
              "Row_index",
              "Column_index",
              "Row_node",
              "Column_node"
            ) %in% names(layer$data)
          ) &&
          !"Direction" %in% names(layer$data)
      },
      logical(1)
    )
  ]
  
  impossible_indices[1L]
}


test_that("plot_network_changes_matrix returns a ggplot object", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(changes)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot works for interaction matrices", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  expect_no_error(
    plot_network_changes_matrix(changes)
  )
  
  p <- plot_network_changes_matrix(changes)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot works for adjacency matrices", {
  
  changes <- make_unipartite_matrix_changes()
  
  expect_no_error(
    plot_network_changes_matrix(changes)
  )
  
  p <- plot_network_changes_matrix(changes)
  
  expect_s3_class(p, "ggplot")
})


test_that("requested row guild is passed to the matrix builder", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  guilds <- sort(
    unique(as.character(changes$nodes$Guild))
  )
  
  expect_no_error(
    plot_network_changes_matrix(
      changes,
      row_guild = guilds[2L]
    )
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      row_guild = "not_a_guild"
    ),
    "`row_guild` must identify one of the two guilds"
  )
})


test_that("strength symbols are only drawn for persistent changed links", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    show_strength_symbols = TRUE
  )
  
  built <- ggplot2::ggplot_build(p)
  
  # Tile layer + point layer + possible boundary layers.
  point_layers <- vapply(
    p$layers,
    function(layer) {
      inherits(layer$geom, "GeomPoint")
    },
    logical(1)
  )
  
  expect_true(any(point_layers))
  
  point_index <- which(point_layers)[1L]
  
  symbol_data <- built$data[[point_index]]
  
  matrix_data <- build_network_change_matrix(changes)
  
  expected_symbols <- matrix_data$cells[
    matrix_data$cells$Direction == "persistent" &
      matrix_data$cells$Strength_direction %in%
      c("strengthened", "weakened"),
    ,
    drop = FALSE
  ]
  
  expect_equal(
    nrow(symbol_data),
    nrow(expected_symbols)
  )
})


test_that("unchanged persistent links do not receive strength symbols", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  matrix_data <- build_network_change_matrix(changes)
  
  unchanged <- matrix_data$cells[
    matrix_data$cells$Direction == "persistent" &
      matrix_data$cells$Strength_direction == "unchanged",
    ,
    drop = FALSE
  ]
  
  expect_gt(nrow(unchanged), 0L)
  
  p <- plot_network_changes_matrix(
    changes,
    show_strength_symbols = TRUE
  )
  
  built <- ggplot2::ggplot_build(p)
  
  point_layers <- vapply(
    p$layers,
    function(layer) {
      inherits(layer$geom, "GeomPoint")
    },
    logical(1)
  )
  
  point_index <- which(point_layers)[1L]
  
  symbol_data <- built$data[[point_index]]
  
  for (i in seq_len(nrow(unchanged))) {
    
    expect_false(any(
      symbol_data$x == unchanged$Column_index[i] &
        symbol_data$y == unchanged$Row_index[i]
    ))
  }
})


test_that("show_strength_symbols FALSE removes the symbol layer", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    show_strength_symbols = FALSE
  )
  
  point_layers <- vapply(
    p$layers,
    function(layer) {
      inherits(layer$geom, "GeomPoint")
    },
    logical(1)
  )
  
  expect_false(any(point_layers))
})


test_that("strength encoding is disabled by default", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(changes)
  
  built <- ggplot2::ggplot_build(p)
  
  tile_index <- get_interaction_tile_layer(p)
  
  tile_data <- built$data[[tile_index]]
  
  expect_true(all(tile_data$alpha == 1))
})


test_that("strength encoding produces alpha values between min_alpha and one", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    encode_strength = TRUE,
    min_alpha = 0.2
  )
  
  built <- ggplot2::ggplot_build(p)
  
  tile_index <- get_interaction_tile_layer(p)
  
  alpha <- built$data[[tile_index]]$alpha
  
  expect_true(all(alpha >= 0.2))
  expect_true(all(alpha <= 1))
  expect_true(length(unique(alpha)) > 1L)
})


test_that("strength_max limits opacity scaling", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    encode_strength = TRUE,
    strength_max = 1,
    min_alpha = 0.2
  )
  
  built <- ggplot2::ggplot_build(p)
  
  tile_index <- get_interaction_tile_layer(p)
  
  alpha <- built$data[[tile_index]]$alpha
  
  expect_true(all(alpha <= 1))
  expect_true(any(alpha == 1))
})


test_that("binary networks reject quantitative strength encoding", {
  
  changes <- make_binary_changes()
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      encode_strength = TRUE
    ),
    "`encode_strength = TRUE` is only meaningful for weighted networks"
  )
})


test_that("node labels can be hidden", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    show_node_labels = FALSE
  )
  
  x_scale <- p$scales$get_scales("x")
  y_scale <- p$scales$get_scales("y")
  
  expect_null(x_scale$breaks)
  expect_null(y_scale$breaks)
})


test_that("node labels are present by default", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(
    changes,
    show_node_labels = TRUE
  )
  
  x_scale <- p$scales$get_scales("x")
  y_scale <- p$scales$get_scales("y")
  
  expect_equal(
    x_scale$breaks,
    seq_len(
      nrow(
        build_network_change_matrix(changes)$column_nodes
      )
    )
  )
  
  expect_equal(
    y_scale$breaks,
    seq_len(
      nrow(
        build_network_change_matrix(changes)$row_nodes
      )
    )
  )
})


test_that("logical plotting arguments are validated", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      show_node_labels = NA
    ),
    "`show_node_labels` must be a single non-missing logical value"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      show_strength_symbols = "yes"
    ),
    "`show_strength_symbols` must be a single non-missing logical value"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      encode_strength = c(TRUE, FALSE)
    ),
    "`encode_strength` must be a single non-missing logical value"
  )
})


test_that("min_alpha is validated", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      min_alpha = -0.1
    ),
    "`min_alpha` must be a single finite numeric value between 0 and 1"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      min_alpha = 1.1
    ),
    "`min_alpha` must be a single finite numeric value between 0 and 1"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      min_alpha = NA_real_
    ),
    "`min_alpha` must be a single finite numeric value between 0 and 1"
  )
})


test_that("strength_max is validated", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      strength_max = 0
    ),
    "`strength_max` must be `NULL` or a single finite positive numeric value"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      strength_max = -1
    ),
    "`strength_max` must be `NULL` or a single finite positive numeric value"
  )
  
  expect_error(
    plot_network_changes_matrix(
      changes,
      strength_max = c(1, 2)
    ),
    "`strength_max` must be `NULL` or a single finite positive numeric value"
  )
})

test_that("status boundaries are plotted at the positions returned by the builder", {

  changes <- make_undirected_two_guild_matrix_changes()

  matrix_data <- build_network_change_matrix(changes)
  p <- plot_network_changes_matrix(changes)

  hline_layers <- p$layers[
    vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomHline")
      },
      logical(1)
    )
  ]

  vline_layers <- p$layers[
    vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomVline")
      },
      logical(1)
    )
  ]

  get_hline_intercepts <- function(layer) {

    if (
      is.data.frame(layer$data) &&
      "yintercept" %in% names(layer$data)
    ) {
      return(layer$data$yintercept)
    }

    if (!is.null(layer$geom_params$yintercept)) {
      return(layer$geom_params$yintercept)
    }

    numeric(0)
  }

  get_vline_intercepts <- function(layer) {

    if (
      is.data.frame(layer$data) &&
      "xintercept" %in% names(layer$data)
    ) {
      return(layer$data$xintercept)
    }

    if (!is.null(layer$geom_params$xintercept)) {
      return(layer$geom_params$xintercept)
    }

    numeric(0)
  }

  observed_h <- unlist(
    lapply(
      hline_layers,
      get_hline_intercepts
    )
  )

  observed_v <- unlist(
    lapply(
      vline_layers,
      get_vline_intercepts
    )
  )

  expect_true(all(
    matrix_data$row_boundaries %in% observed_h
  ))

  expect_true(all(
    matrix_data$column_boundaries %in% observed_v
  ))
})

test_that("matrix plot includes a panel border", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(changes)
  
  expect_s3_class(
    p$theme$panel.border,
    "element_rect"
  )
})


test_that("structurally impossible cells are plotted", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  matrix_data <- build_network_change_matrix(changes)
  
  expect_gt(
    nrow(matrix_data$impossible_cells),
    0L
  )
  
  p <- plot_network_changes_matrix(changes)
  
  impossible_index <- get_impossible_tile_layer(p)
  
  expect_false(is.na(impossible_index))
  
  plotted <- p$layers[[impossible_index]]$data
  
  expect_equal(
    nrow(plotted),
    nrow(matrix_data$impossible_cells)
  )
  
  expected_keys <- paste(
    matrix_data$impossible_cells$Column_index,
    matrix_data$impossible_cells$Row_index
  )
  
  observed_keys <- paste(
    plotted$Column_index,
    plotted$Row_index
  )
  
  expect_setequal(
    observed_keys,
    expected_keys
  )
})


test_that("structurally impossible cells use a light grey background", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(changes)
  
  impossible_index <- get_impossible_tile_layer(p)
  
  layer <- p$layers[[impossible_index]]
  
  expect_identical(
    layer$aes_params$fill,
    "grey90"
  )
})


test_that("impossible cells are drawn below observed interactions", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  p <- plot_network_changes_matrix(changes)
  
  impossible_index <- get_impossible_tile_layer(p)
  interaction_index <- get_interaction_tile_layer(p)
  
  expect_lt(
    impossible_index,
    interaction_index
  )
})