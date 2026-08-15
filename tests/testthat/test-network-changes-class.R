# =============================================================================
# Tests for network_changes class
# =============================================================================

test_that("constructor creates and validates a network_changes object", {
  changes <- make_weighted_changes()

  expect_s3_class(changes, "network_changes")
  expect_named(
    changes,
    c("comparison", "structural", "links", "nodes")
  )

  expect_no_error(validate_network_changes(changes))
})


test_that("validator detects missing components and malformed component types", {
  changes <- make_weighted_changes()

  missing_component <- changes
  missing_component$links <- NULL

  expect_error(
    validate_network_changes(missing_component),
    "Missing components"
  )

  malformed_comparison <- changes
  malformed_comparison$comparison <- "invalid"

  expect_error(
    validate_network_changes(malformed_comparison),
    "`comparison` must be a list"
  )

  malformed_structural <- changes
  malformed_structural$structural <- list()

  expect_error(
    validate_network_changes(malformed_structural),
    "`structural` must be a data frame"
  )
})


test_that("validator detects inconsistent counts across object levels", {
  changes <- make_weighted_changes()

  broken_structural <- changes
  broken_structural$structural$n_links[1L] <-
    broken_structural$structural$n_links[1L] + 1L

  expect_error(
    validate_network_changes(broken_structural),
    "structural link counts do not equal"
  )

  broken_nodes_union <- changes
  broken_nodes_union$comparison$n_nodes_union <-
    broken_nodes_union$comparison$n_nodes_union + 1L

  expect_error(
    validate_network_changes(broken_nodes_union),
    "n_nodes_union"
  )

  broken_links_union <- changes
  broken_links_union$comparison$n_links_union <-
    broken_links_union$comparison$n_links_union + 1L

  expect_error(
    validate_network_changes(broken_links_union),
    "n_links_union"
  )

  broken_nodes_1 <- changes
  broken_nodes_1$comparison$n_nodes_network_1 <-
    broken_nodes_1$comparison$n_nodes_network_1 + 1L

  expect_error(
    validate_network_changes(broken_nodes_1),
    "n_nodes_network_1"
  )

  broken_links_1 <- changes
  broken_links_1$comparison$n_links_network_1 <-
    broken_links_1$comparison$n_links_network_1 + 1L

  expect_error(
    validate_network_changes(broken_links_1),
    "n_links_network_1"
  )
})


test_that("print.network_changes returns invisibly and prints key metadata", {
  changes <- make_weighted_changes()

  output <- capture.output(
    returned <- print(changes)
  )

  expect_identical(returned, changes)

  expect_true(any(grepl("<network_changes>", output, fixed = TRUE)))
  expect_true(any(grepl("Comparison: temporal", output, fixed = TRUE)))
  expect_true(any(grepl("Nodes", output, fixed = TRUE)))
  expect_true(any(grepl("Links", output, fixed = TRUE)))
  expect_true(any(grepl("Weighted:", output, fixed = TRUE)))

  # n_changed_links is no longer part of the comparison metadata.
  expect_false(any(grepl("Changed:", output, fixed = TRUE)))
})


test_that("summary.network_changes returns the expected summary object", {
  changes <- make_weighted_changes()
  result <- summary(changes)

  expect_s3_class(result, "summary.network_changes")
  expect_named(
    result,
    c("comparison", "structural")
  )

  expect_equal(result$comparison, changes$comparison)
  expect_equal(result$structural, changes$structural)

  expect_true(
    all(
      c(
        "Direction",
        "Mechanism",
        "Strength_direction",
        "n_links"
      ) %in% names(result$structural)
    )
  )

  expect_false(any(
    c(
      "proportion_all_links",
      "proportion_within_direction",
      "proportion_changed_links"
    ) %in% names(result$structural)
  ))
})


test_that("print.summary.network_changes returns invisibly and prints link-change table", {
  changes <- make_weighted_changes()
  result <- summary(changes)

  output <- capture.output(
    returned <- print(result)
  )

  expect_identical(returned, result)

  expect_true(any(grepl("Summary of network changes", output, fixed = TRUE)))
  expect_true(any(grepl("Comparison type: temporal", output, fixed = TRUE)))
  expect_true(any(grepl("Link changes", output, fixed = TRUE)))
  expect_true(any(grepl("Strength_direction", output, fixed = TRUE)))
  expect_true(any(grepl("n_links", output, fixed = TRUE)))
})


test_that("weighted and binary objects both pass class validation", {
  weighted <- make_weighted_changes()
  binary <- make_binary_changes()

  expect_valid_network_changes(weighted)
  expect_valid_network_changes(binary)

  expect_no_error(validate_network_changes(weighted))
  expect_no_error(validate_network_changes(binary))

  expect_equal(nrow(weighted$structural), 9L)
  expect_equal(nrow(binary$structural), 7L)

  expect_true("Strength_direction" %in% names(weighted$structural))
  expect_true("Strength_direction" %in% names(binary$structural))
  expect_true("Strength_direction" %in% names(weighted$links))
  expect_true("Strength_direction" %in% names(binary$links))
})
