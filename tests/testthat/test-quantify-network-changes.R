# =============================================================================
# Tests for quantify_network_changes()
# =============================================================================

test_that("quantification returns the expected object structure", {
  changes <- make_weighted_changes()

  expect_valid_network_changes(changes)
  expect_named(
    changes,
    c("comparison", "structural", "links", "nodes")
  )
  expect_s3_class(changes$structural, "data.frame")
  expect_s3_class(changes$links, "data.frame")
  expect_s3_class(changes$nodes, "data.frame")
})


test_that("structural counts reproduce the link-level contingency table", {
  changes <- make_weighted_changes()

  for (i in seq_len(nrow(changes$structural))) {
    row <- changes$structural[i, , drop = FALSE]

    observed <- sum(
      changes$links$Direction == row$Direction &
        changes$links$Mechanism == row$Mechanism &
        changes$links$Strength_direction == row$Strength_direction
    )

    expect_equal(row$n_links, observed)
  }

  expect_equal(
    sum(changes$structural$n_links),
    nrow(changes$links)
  )
})


test_that("comparison metadata preserve self-link permissions", {
  
  prepared <- make_weighted_prepared()
  
  changes <- quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  
  expect_identical(
    changes$comparison$allow_self_links,
    prepared$network_type$allow_self_links
  )
})


test_that("unipartite self-link permissions are preserved", {
  
  dat <- data.frame(
    Site = c(
      "site_1", "site_1",
      "site_1", "site_1"
    ),
    Time = c(
      "early", "early",
      "late", "late"
    ),
    Origin_node = c(
      "A", "A",
      "A", "A"
    ),
    Destiny_node = c(
      "A", "B",
      "A", "B"
    ),
    Strength = c(
      2, 3,
      4, 5
    )
  )
  
  prepared <- prepare_interactions(
    dat,
    directed = FALSE,
    multipartite = FALSE,
    allow_self_links = TRUE
  )
  
  changes <- quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  
  expect_true(
    prepared$network_type$allow_self_links
  )
  
  expect_true(
    changes$comparison$allow_self_links
  )
})


test_that("weighted networks contain all nine link-change categories in fixed order", {
  structural <- make_weighted_changes()$structural

  expect_equal(nrow(structural), 9L)

  expect_identical(
    paste(
      structural$Direction,
      structural$Mechanism,
      structural$Strength_direction
    ),
    c(
      "persistent permanent unchanged",
      "persistent permanent weakened",
      "persistent permanent strengthened",
      "lost rewiring weakened",
      "lost one_non_permanent_node weakened",
      "lost two_non_permanent_nodes weakened",
      "gained rewiring strengthened",
      "gained one_non_permanent_node strengthened",
      "gained two_non_permanent_nodes strengthened"
    )
  )

  expect_identical(
    structural$n_links,
    c(0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
})


test_that("binary networks contain all seven valid link-change categories", {
  structural <- make_binary_changes()$structural

  expect_equal(nrow(structural), 7L)

  expect_identical(
    paste(
      structural$Direction,
      structural$Mechanism,
      structural$Strength_direction
    ),
    c(
      "persistent permanent unchanged",
      "lost rewiring weakened",
      "lost one_non_permanent_node weakened",
      "lost two_non_permanent_nodes weakened",
      "gained rewiring strengthened",
      "gained one_non_permanent_node strengthened",
      "gained two_non_permanent_nodes strengthened"
    )
  )

  expect_identical(
    structural$n_links,
    c(2L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
})


test_that("structural output contains counts but no proportion columns", {
  structural <- make_weighted_changes()$structural

  expect_true("n_links" %in% names(structural))
  expect_false(any(
    c(
      "proportion_all_links",
      "proportion_within_direction",
      "proportion_changed_links"
    ) %in% names(structural)
  ))
})


test_that("comparison counts are conserved across all levels", {
  changes <- make_weighted_changes()

  expect_equal(
    changes$comparison$n_nodes_network_1,
    sum(changes$nodes$Present_network_1)
  )
  expect_equal(
    changes$comparison$n_nodes_network_2,
    sum(changes$nodes$Present_network_2)
  )
  expect_equal(
    changes$comparison$n_nodes_union,
    nrow(changes$nodes)
  )

  expect_equal(
    changes$comparison$n_links_network_1,
    sum(changes$links$Present_network_1)
  )
  expect_equal(
    changes$comparison$n_links_network_2,
    sum(changes$links$Present_network_2)
  )
  expect_equal(
    changes$comparison$n_links_union,
    nrow(changes$links)
  )
})


test_that("weighted link-level changes are calculated correctly", {
  links <- make_weighted_changes()$links

  expect_equal(
    links$Strength_change,
    links$Strength_network_2 - links$Strength_network_1
  )
  expect_equal(
    links$Absolute_strength_change,
    abs(links$Strength_change)
  )

  expect_true(all(
    links$Strength_direction[links$Direction == "gained"] ==
      "strengthened"
  ))
  expect_true(all(
    links$Strength_direction[links$Direction == "lost"] ==
      "weakened"
  ))

  persistent <- links$Direction == "persistent"
  expect_setequal(
    links$Strength_direction[persistent],
    c("strengthened", "weakened")
  )
})


test_that("strength tolerance applies only to persistent links", {
  changes <- make_weighted_changes(strength_tolerance = 3)
  links <- changes$links

  persistent <- links$Direction == "persistent"
  expect_true(all(
    links$Strength_direction[persistent] == "unchanged"
  ))

  expect_true(all(
    links$Strength_direction[links$Direction == "gained"] ==
      "strengthened"
  ))
  expect_true(all(
    links$Strength_direction[links$Direction == "lost"] ==
      "weakened"
  ))
})


test_that("weighted structural totals and means equal link-level calculations", {
  changes <- make_weighted_changes()

  for (i in seq_len(nrow(changes$structural))) {
    row <- changes$structural[i, , drop = FALSE]
    selected <-
      changes$links$Direction == row$Direction &
      changes$links$Mechanism == row$Mechanism &
      changes$links$Strength_direction == row$Strength_direction

    expect_equal(
      row$total_strength_network_1,
      sum(changes$links$Strength_network_1[selected])
    )
    expect_equal(
      row$total_strength_network_2,
      sum(changes$links$Strength_network_2[selected])
    )
    expect_equal(
      row$net_strength_change,
      sum(changes$links$Strength_change[selected])
    )
    expect_equal(
      row$total_absolute_strength_change,
      sum(changes$links$Absolute_strength_change[selected])
    )

    if (any(selected)) {
      expect_equal(
        row$mean_strength_change,
        mean(changes$links$Strength_change[selected])
      )
      expect_equal(
        row$mean_absolute_strength_change,
        mean(changes$links$Absolute_strength_change[selected])
      )
    } else {
      expect_true(is.na(row$mean_strength_change))
      expect_true(is.na(row$mean_absolute_strength_change))
    }
  }
})


test_that("known weighted category values are correct", {
  changes <- make_weighted_changes()
  structural <- changes$structural
  links <- changes$links

  weakened_persistent <- structural[
    structural$Direction == "persistent" &
      structural$Mechanism == "permanent" &
      structural$Strength_direction == "weakened",
    ,
    drop = FALSE
  ]

  strengthened_persistent <- structural[
    structural$Direction == "persistent" &
      structural$Mechanism == "permanent" &
      structural$Strength_direction == "strengthened",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(weakened_persistent), 1L)
  expect_equal(nrow(strengthened_persistent), 1L)
  expect_equal(weakened_persistent$n_links, 1L)
  expect_equal(strengthened_persistent$n_links, 1L)
  expect_lt(weakened_persistent$net_strength_change, 0)
  expect_gt(strengthened_persistent$net_strength_change, 0)

  lost_rewiring <- structural[
    structural$Direction == "lost" &
      structural$Mechanism == "rewiring" &
      structural$Strength_direction == "weakened",
    ,
    drop = FALSE
  ]
  expect_equal(lost_rewiring$net_strength_change, -3)

  gained_rewiring <- structural[
    structural$Direction == "gained" &
      structural$Mechanism == "rewiring" &
      structural$Strength_direction == "strengthened",
    ,
    drop = FALSE
  ]
  expect_equal(gained_rewiring$net_strength_change, 7)

  expect_equal(
    sum(structural$n_links),
    nrow(links)
  )
})


test_that("binary results retain Strength_direction but omit weighted change metrics", {
  binary <- make_binary_changes()

  weighted_structural_columns <- c(
    "total_strength_network_1",
    "total_strength_network_2",
    "net_strength_change",
    "total_absolute_strength_change",
    "mean_strength_change",
    "mean_absolute_strength_change"
  )

  weighted_link_columns <- c(
    "Strength_change",
    "Absolute_strength_change"
  )

  expect_true("Strength_direction" %in% names(binary$structural))
  expect_true("Strength_direction" %in% names(binary$links))

  expect_false(any(
    weighted_structural_columns %in% names(binary$structural)
  ))
  expect_false(any(
    weighted_link_columns %in% names(binary$links)
  ))

  expect_true(all(
    binary$links$Strength_direction[
      binary$links$Direction == "persistent"
    ] == "unchanged"
  ))
  expect_true(all(
    binary$links$Strength_direction[
      binary$links$Direction == "lost"
    ] == "weakened"
  ))
  expect_true(all(
    binary$links$Strength_direction[
      binary$links$Direction == "gained"
    ] == "strengthened"
  ))
})


test_that("binary and weighted inputs preserve the same Direction-Mechanism totals", {
  weighted <- make_weighted_changes()
  binary <- make_binary_changes()

  collapse_categories <- function(structural) {
    totals <- stats::aggregate(
      n_links ~ Direction + Mechanism,
      data = structural,
      FUN = sum
    )

    order_key <- match(
      paste(totals$Direction, totals$Mechanism),
      c(
        "persistent permanent",
        "lost rewiring",
        "lost one_non_permanent_node",
        "lost two_non_permanent_nodes",
        "gained rewiring",
        "gained one_non_permanent_node",
        "gained two_non_permanent_nodes"
      )
    )

    totals[order(order_key), , drop = FALSE]
  }

  expect_equal(
    collapse_categories(binary$structural),
    collapse_categories(weighted$structural)
  )
})


test_that("precomputed node and link classifications reproduce automatic output", {
  prepared <- make_weighted_prepared()

  nodes <- classify_network_nodes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  links <- classify_network_links(
    prepared,
    "site_1", "early",
    "site_1", "late",
    node_status = nodes
  )

  automatic <- quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  precomputed <- quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late",
    node_classification = nodes,
    link_classification = links
  )

  expect_equal(precomputed$comparison, automatic$comparison)
  expect_equal(precomputed$nodes, automatic$nodes)
  expect_equal(precomputed$links, automatic$links)
  expect_equal(precomputed$structural, automatic$structural)
})


test_that("inconsistent precomputed node and link classifications fail", {
  prepared <- make_weighted_prepared()

  nodes <- classify_network_nodes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  links <- classify_network_links(
    prepared,
    "site_1", "early",
    "site_1", "late",
    node_status = nodes
  )

  extra_node <- nodes[1, , drop = FALSE]
  extra_node$Node <- "invented_node"
  extra_node$Guild <- "plant"
  extra_nodes <- rbind(nodes, extra_node)

  expect_error(
    quantify_network_changes(
      prepared,
      "site_1", "early",
      "site_1", "late",
      node_classification = extra_nodes,
      link_classification = links
    ),
    "do not contain the same set of nodes"
  )
})


test_that("invalid strength tolerance is rejected", {
  prepared <- make_weighted_prepared()

  for (value in list(-1, NA_real_, Inf, c(0, 1), "zero")) {
    expect_error(
      quantify_network_changes(
        prepared,
        "site_1", "early",
        "site_1", "late",
        strength_tolerance = value
      ),
      "finite, non-negative"
    )
  }
})
