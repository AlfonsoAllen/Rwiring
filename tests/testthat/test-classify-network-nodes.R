# =============================================================================
# Tests for classify_network_nodes()
# =============================================================================

test_that("nodes are classified correctly in a temporal comparison", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    site_1 = "site_1",
    time_1 = "early",
    site_2 = "site_1",
    time_2 = "late"
  )

  expect_s3_class(nodes, "data.frame")
  expect_equal(unique(nodes$Comparison_type), "temporal")
  expect_equal(anyDuplicated(nodes$Node), 0L)

  expected_nodes <- c(
    "bee_1", "bee_2",
    "bee_gained_1", "bee_gained_2",
    "bee_lost_1", "bee_lost_2",
    "plant_1", "plant_2",
    "plant_gained", "plant_lost"
  )
  expect_setequal(nodes$Node, expected_nodes)

  expected_status <- c(
    bee_1 = "permanent",
    bee_2 = "permanent",
    bee_gained_1 = "gained",
    bee_gained_2 = "gained",
    bee_lost_1 = "lost",
    bee_lost_2 = "lost",
    plant_1 = "permanent",
    plant_2 = "permanent",
    plant_gained = "gained",
    plant_lost = "lost"
  )

  observed <- stats::setNames(nodes$Node_status, nodes$Node)
  expect_identical(unname(observed[names(expected_status)]),
                   unname(expected_status))

  expect_equal(sum(nodes$Present_network_1), 7L)
  expect_equal(sum(nodes$Present_network_2), 7L)
  expect_equal(table(nodes$Node_status)[["permanent"]], 4L)
  expect_equal(table(nodes$Node_status)[["lost"]], 3L)
  expect_equal(table(nodes$Node_status)[["gained"]], 3L)
})


test_that("guild information is retained for every node", {
  nodes <- classify_network_nodes(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  expect_false(anyNA(nodes$Guild))
  expect_true(all(nodes$Guild[grepl("^plant", nodes$Node)] == "plant"))
  expect_true(all(nodes$Guild[grepl("^bee", nodes$Node)] == "pollinator"))
})


test_that("comparison type is inferred correctly", {
  spatial <- prepare_interactions(
    make_spatial_change_data(),
    directed = TRUE,
    multipartite = TRUE
  )

  spatial_nodes <- classify_network_nodes(
    spatial,
    "site_1", "survey",
    "site_2", "survey"
  )
  expect_equal(unique(spatial_nodes$Comparison_type), "spatial")

  spatiotemporal <- prepare_interactions(
    make_spatiotemporal_change_data(),
    directed = TRUE,
    multipartite = TRUE
  )

  spatiotemporal_nodes <- classify_network_nodes(
    spatiotemporal,
    "site_1", "early",
    "site_2", "late"
  )
  expect_equal(
    unique(spatiotemporal_nodes$Comparison_type),
    "spatiotemporal"
  )
})


test_that("reversing the comparison swaps lost and gained nodes", {
  prepared <- make_weighted_prepared()

  forward <- classify_network_nodes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  reverse <- classify_network_nodes(
    prepared,
    "site_1", "late",
    "site_1", "early"
  )

  reverse_lookup <- stats::setNames(reverse$Node_status, reverse$Node)

  expected_reverse <- ifelse(
    forward$Node_status == "lost",
    "gained",
    ifelse(
      forward$Node_status == "gained",
      "lost",
      "permanent"
    )
  )

  expect_identical(
    unname(reverse_lookup[forward$Node]),
    expected_reverse
  )
})


test_that("comparison arguments and requested networks are validated", {
  prepared <- make_weighted_prepared()

  expect_error(
    classify_network_nodes(
      prepared,
      c("site_1", "site_2"), "early",
      "site_1", "late"
    ),
    "single non-missing value"
  )

  expect_error(
    classify_network_nodes(
      prepared,
      "site_1", NA_character_,
      "site_1", "late"
    ),
    "single non-missing value"
  )

  expect_error(
    classify_network_nodes(
      prepared,
      "site_1", "early",
      "site_1", "early"
    ),
    "different Site-Time combinations"
  )

  expect_error(
    classify_network_nodes(
      prepared,
      "missing_site", "early",
      "site_1", "late"
    ),
    "No interaction data"
  )
})
