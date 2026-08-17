# =============================================================================
# Tests for build_network_change_matrix()
# =============================================================================

# -------------------------------------------------------------------------
# General output
# -------------------------------------------------------------------------

test_that("matrix builder returns the expected components", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_named(
    result,
    c(
      "layout",
      "directed",
      "row_guild",
      "column_guild",
      "row_nodes",
      "column_nodes",
      "cells",
      "impossible_cells",
      "row_boundaries",
      "column_boundaries",
      "row_guild_boundaries",
      "column_guild_boundaries"
    )
  )
  
  expect_s3_class(result$row_nodes, "data.frame")
  expect_s3_class(result$column_nodes, "data.frame")
  expect_s3_class(result$cells, "data.frame")
  expect_s3_class(result$impossible_cells, "data.frame")
})


# -------------------------------------------------------------------------
# Unipartite networks
# -------------------------------------------------------------------------

test_that("unipartite networks use an adjacency matrix", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_identical(
    result$layout,
    "adjacency"
  )
  
  expect_false(result$directed)
  
  expect_null(result$row_guild)
  expect_null(result$column_guild)
  
  expect_identical(
    as.character(result$row_nodes$Node),
    as.character(result$column_nodes$Node)
  )
})


test_that("unipartite adjacency nodes are ordered by status and name", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_identical(
    as.character(result$row_nodes$Node),
    c(
      "node_a",
      "node_b",
      "node_c",
      "node_d"
    )
  )
  
  expect_identical(
    as.character(result$row_nodes$Node_status),
    c(
      "permanent",
      "permanent",
      "lost",
      "gained"
    )
  )
  
  expect_status_order(result$row_nodes)
})


test_that("undirected adjacency matrices mirror every off-diagonal link", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  n_self_links <- sum(
    changes$links$Origin_node ==
      changes$links$Destiny_node
  )
  
  expected_n_cells <-
    2L * nrow(changes$links) -
    n_self_links
  
  expect_equal(
    nrow(result$cells),
    expected_n_cells
  )
  
  for (i in seq_len(nrow(changes$links))) {
    
    origin <- as.character(
      changes$links$Origin_node[i]
    )
    
    destiny <- as.character(
      changes$links$Destiny_node[i]
    )
    
    forward <- with(
      result$cells,
      Row_node == origin &
        Column_node == destiny
    )
    
    expect_true(any(forward))
    
    if (origin != destiny) {
      
      reverse <- with(
        result$cells,
        Row_node == destiny &
          Column_node == origin
      )
      
      expect_true(any(reverse))
    }
  }
})


test_that("adjacency cell indices match node positions", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  row_lookup <- stats::setNames(
    seq_len(nrow(result$row_nodes)),
    as.character(result$row_nodes$Node)
  )
  
  column_lookup <- stats::setNames(
    seq_len(nrow(result$column_nodes)),
    as.character(result$column_nodes$Node)
  )
  
  expect_identical(
    result$cells$Row_index,
    unname(row_lookup[result$cells$Row_node])
  )
  
  expect_identical(
    result$cells$Column_index,
    unname(column_lookup[result$cells$Column_node])
  )
})


test_that("observed links are never classified as impossible cells in unipartite matrices", {
  
  changes <- make_unipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  cell_keys <- paste(
    result$cells$Row_index,
    result$cells$Column_index
  )
  
  impossible_keys <- paste(
    result$impossible_cells$Row_index,
    result$impossible_cells$Column_index
  )
  
  expect_length(
    intersect(cell_keys, impossible_keys),
    0L
  )
})

# -------------------------------------------------------------------------
# Two-guild undirected multipartite networks
# -------------------------------------------------------------------------

test_that("undirected two-guild multipartite networks use an interaction matrix", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  guilds <- sort(
    unique(as.character(changes$nodes$Guild))
  )
  
  result <- build_network_change_matrix(
    changes,
    row_guild = guilds[1L]
  )
  
  expect_identical(
    result$layout,
    "interaction"
  )
  
  expect_false(result$directed)
  
  expect_identical(
    result$row_guild,
    guilds[1L]
  )
  
  expect_identical(
    result$column_guild,
    guilds[2L]
  )
})


test_that("interaction matrix places guilds on the requested axes", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  guilds <- sort(
    unique(as.character(changes$nodes$Guild))
  )
  
  result <- build_network_change_matrix(
    changes,
    row_guild = guilds[2L]
  )
  
  expect_true(all(
    as.character(result$row_nodes$Guild) ==
      guilds[2L]
  ))
  
  expect_true(all(
    as.character(result$column_nodes$Guild) ==
      guilds[1L]
  ))
  
  expect_true(all(
    result$cells$Row_node %in%
      as.character(result$row_nodes$Node)
  ))
  
  expect_true(all(
    result$cells$Column_node %in%
      as.character(result$column_nodes$Node)
  ))
})


test_that("interaction matrices contain one cell per classified link", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_equal(
    nrow(result$cells),
    nrow(changes$links)
  )
  
  expect_equal(
    sum(result$cells$Direction == "persistent"),
    sum(changes$links$Direction == "persistent")
  )
  
  expect_equal(
    sum(result$cells$Direction == "lost"),
    sum(changes$links$Direction == "lost")
  )
  
  expect_equal(
    sum(result$cells$Direction == "gained"),
    sum(changes$links$Direction == "gained")
  )
})


test_that("interaction matrices preserve link-change classifications", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_true(all(
    c(
      "Direction",
      "Mechanism",
      "Strength_direction"
    ) %in% names(result$cells)
  ))
  
  expect_setequal(
    result$cells$Direction,
    changes$links$Direction
  )
  
  expect_setequal(
    result$cells$Mechanism,
    changes$links$Mechanism
  )
  
  expect_setequal(
    result$cells$Strength_direction,
    changes$links$Strength_direction
  )
})


test_that("row and column nodes of interaction matrices follow status ordering", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_status_order(result$row_nodes)
  expect_status_order(result$column_nodes)
})


test_that("invalid row guilds are rejected for interaction matrices", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  expect_error(
    build_network_change_matrix(
      changes,
      row_guild = "not_a_guild"
    ),
    "`row_guild` must identify one of the two guilds"
  )
})


# -------------------------------------------------------------------------
# Structurally impossible cells
# -------------------------------------------------------------------------


test_that("lost-gained node combinations are structurally impossible", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  row_status <- stats::setNames(
    as.character(result$row_nodes$Node_status),
    as.character(result$row_nodes$Node)
  )
  
  column_status <- stats::setNames(
    as.character(result$column_nodes$Node_status),
    as.character(result$column_nodes$Node)
  )
  
  observed_pairs <- paste(
    row_status[result$impossible_cells$Row_node],
    column_status[result$impossible_cells$Column_node],
    sep = " -> "
  )
  
  expect_true(
    all(
      observed_pairs %in%
        c(
          "lost -> gained",
          "gained -> lost"
        )
    )
  )
  
  expect_true(
    any(observed_pairs == "lost -> gained") ||
      any(observed_pairs == "gained -> lost")
  )
})


test_that("interaction matrices identify all lost-gained combinations", {
  
  changes <- make_undirected_two_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expected <- expand.grid(
    Row_index = seq_len(nrow(result$row_nodes)),
    Column_index = seq_len(nrow(result$column_nodes)),
    KEEP.OUT.ATTRS = FALSE
  )
  
  row_status <- as.character(
    result$row_nodes$Node_status[expected$Row_index]
  )
  
  column_status <- as.character(
    result$column_nodes$Node_status[expected$Column_index]
  )
  
  expected <- expected[
    (
      row_status == "lost" &
        column_status == "gained"
    ) |
      (
        row_status == "gained" &
          column_status == "lost"
      ),
    ,
    drop = FALSE
  ]
  
  observed <- result$impossible_cells[
    ,
    c("Row_index", "Column_index"),
    drop = FALSE
  ]
  
  expect_setequal(
    paste(expected$Row_index, expected$Column_index),
    paste(observed$Row_index, observed$Column_index)
  )
})


test_that("unipartite adjacency diagonal is impossible when self-links are not allowed", {
  
  changes <- make_unipartite_matrix_changes()
  
  expect_false(
    changes$comparison$allow_self_links
  )
  
  result <- build_network_change_matrix(changes)
  
  diagonal <- result$impossible_cells[
    result$impossible_cells$Row_node ==
      result$impossible_cells$Column_node,
    ,
    drop = FALSE
  ]
  
  expect_equal(
    nrow(diagonal),
    nrow(result$row_nodes)
  )
  
  expect_setequal(
    diagonal$Row_node,
    as.character(result$row_nodes$Node)
  )
})


test_that("unipartite adjacency diagonal remains possible when self-links are allowed", {
  
  changes <- make_self_link_matrix_changes()
  
  expect_true(
    changes$comparison$allow_self_links
  )
  
  result <- build_network_change_matrix(changes)
  
  diagonal_impossible <- with(
    result$impossible_cells,
    Row_node == Column_node
  )
  
  expect_false(
    any(diagonal_impossible)
  )
})


test_that("undirected self-links are not duplicated in adjacency matrices", {
  
  changes <- make_self_link_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  self_links <- result$cells[
    result$cells$Row_node ==
      result$cells$Column_node,
    ,
    drop = FALSE
  ]
  
  original_self_links <- changes$links[
    changes$links$Origin_node ==
      changes$links$Destiny_node,
    ,
    drop = FALSE
  ]
  
  expect_equal(
    nrow(self_links),
    nrow(original_self_links)
  )
})


# -------------------------------------------------------------------------
# Directed multipartite networks
# -------------------------------------------------------------------------

test_that("directed multipartite networks use an adjacency matrix", {
  
  changes <- make_directed_multipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_identical(
    result$layout,
    "adjacency"
  )
  
  expect_true(result$directed)
  
  expect_identical(
    as.character(result$row_nodes$Node),
    as.character(result$column_nodes$Node)
  )
})


test_that("directed adjacency matrices are not mirrored", {
  
  changes <- make_directed_multipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_equal(
    nrow(result$cells),
    nrow(changes$links)
  )
  
  for (i in seq_len(nrow(changes$links))) {
    
    origin <- as.character(
      changes$links$Origin_node[i]
    )
    
    destiny <- as.character(
      changes$links$Destiny_node[i]
    )
    
    expect_true(any(
      result$cells$Row_node == origin &
        result$cells$Column_node == destiny
    ))
  }
})


test_that("directed adjacency orientation follows Origin to Destiny", {
  
  changes <- make_directed_multipartite_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_identical(
    result$cells$Row_node,
    as.character(
      result$cells$Origin_node
    )
  )
  
  expect_identical(
    result$cells$Column_node,
    as.character(
      result$cells$Destiny_node
    )
  )
})


# -------------------------------------------------------------------------
# Multipartite networks with more than two guilds
# -------------------------------------------------------------------------

test_that("multipartite networks with more than two guilds use adjacency", {
  
  changes <- make_three_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_identical(
    result$layout,
    "adjacency"
  )
  
  expect_false(result$directed)
})


test_that("multipartite adjacency matrices keep guilds in contiguous blocks", {
  
  changes <- make_three_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  observed_guilds <- as.character(
    result$row_nodes$Guild
  )
  
  guild_runs <- rle(observed_guilds)
  
  expect_identical(
    guild_runs$values,
    sort(unique(observed_guilds))
  )
})


test_that("multipartite adjacency matrices report guild boundaries", {
  
  changes <- make_three_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  expect_equal(
    length(result$row_guild_boundaries),
    2L
  )
  
  expect_identical(
    result$row_guild_boundaries,
    result$column_guild_boundaries
  )
})

test_that("multipartite adjacency matrices mark within-guild cells as impossible", {
  
  changes <- make_three_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  row_guild <- stats::setNames(
    as.character(result$row_nodes$Guild),
    as.character(result$row_nodes$Node)
  )
  
  column_guild <- stats::setNames(
    as.character(result$column_nodes$Guild),
    as.character(result$column_nodes$Node)
  )
  
  impossible <- result$impossible_cells
  
  same_guild <-
    row_guild[impossible$Row_node] ==
    column_guild[impossible$Column_node]
  
  # At least some impossible cells arise from multipartite structure.
  expect_true(any(same_guild))
  
  # Every possible within-guild pair must be marked as impossible.
  all_pairs <- expand.grid(
    Row_index = seq_len(nrow(result$row_nodes)),
    Column_index = seq_len(nrow(result$column_nodes)),
    KEEP.OUT.ATTRS = FALSE
  )
  
  expected_same_guild <-
    as.character(
      result$row_nodes$Guild[all_pairs$Row_index]
    ) ==
    as.character(
      result$column_nodes$Guild[all_pairs$Column_index]
    )
  
  expected <- all_pairs[
    expected_same_guild,
    ,
    drop = FALSE
  ]
  
  observed_keys <- paste(
    impossible$Row_index,
    impossible$Column_index
  )
  
  expected_keys <- paste(
    expected$Row_index,
    expected$Column_index
  )
  
  expect_true(
    all(expected_keys %in% observed_keys)
  )
})


test_that("observed links are never classified as impossible cells in multipartite matrices", {
  
  changes <- make_three_guild_matrix_changes()
  
  result <- build_network_change_matrix(changes)
  
  cell_keys <- paste(
    result$cells$Row_index,
    result$cells$Column_index
  )
  
  impossible_keys <- paste(
    result$impossible_cells$Row_index,
    result$impossible_cells$Column_index
  )
  
  expect_length(
    intersect(cell_keys, impossible_keys),
    0L
  )
})