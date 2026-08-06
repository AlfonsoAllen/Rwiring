# =============================================================================
# Tests for validate_node_classification()
# =============================================================================

test_that("a classification created by classify_network_nodes is valid", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  expect_invisible(
    validate_node_classification(
      nodes,
      "site_1", "early",
      "site_1", "late"
    )
  )
})


test_that("node classification input type, rows, and columns are validated", {
  expect_error(
    validate_node_classification(
      list(),
      "site_1", "early",
      "site_1", "late"
    ),
    "must be a data frame"
  )

  expect_error(
    validate_node_classification(
      data.frame(),
      "site_1", "early",
      "site_1", "late"
    ),
    "at least one node"
  )

  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  broken <- nodes
  broken$Present_network_1 <- NULL

  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "missing required columns"
  )
})


test_that("node presence indicators and statuses must be coherent", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  broken <- nodes
  broken$Present_network_1 <- as.integer(broken$Present_network_1)
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "non-missing logical"
  )

  broken <- nodes
  broken$Present_network_1[1] <- FALSE
  broken$Present_network_2[1] <- FALSE
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "at least one network"
  )

  broken <- nodes
  broken$Node_status[1] <- "lost"
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "inconsistent with the node-presence"
  )

  broken <- nodes
  broken$Node_status[1] <- "invalid"
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "invalid classifications"
  )
})


test_that("node identifiers and uniqueness are validated", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  broken <- nodes
  broken$Node[1] <- ""
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "missing or empty"
  )

  broken <- rbind(nodes, nodes[1, , drop = FALSE])
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "one row per node"
  )
})


test_that("comparison identifiers and type are validated", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  expect_error(
    validate_node_classification(
      nodes,
      "site_1", "late",
      "site_1", "early"
    ),
    "does not correspond"
  )

  broken <- nodes
  broken$Comparison_type <- "spatial"
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "inconsistent with the requested comparison"
  )

  broken <- nodes
  broken$Comparison_type <- "invalid"
  expect_error(
    validate_node_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "contains invalid values"
  )
})
