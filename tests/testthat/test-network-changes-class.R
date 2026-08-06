# =============================================================================
# Tests for the network_changes class and S3 methods
# =============================================================================

test_that("constructor creates and validates a network_changes object", {
  changes <- make_weighted_changes()

  rebuilt <- new_network_changes(
    comparison = changes$comparison,
    structural = changes$structural,
    links = changes$links,
    nodes = changes$nodes
  )

  expect_s3_class(rebuilt, "network_changes")
  expect_equal(rebuilt, changes)
  expect_invisible(validate_network_changes(rebuilt))
})


test_that("validator detects missing components and malformed component types", {
  changes <- make_weighted_changes()

  broken <- changes
  broken$links <- NULL
  expect_error(
    validate_network_changes(broken),
    "Missing components"
  )

  broken <- changes
  broken$structural <- list()
  expect_error(
    validate_network_changes(broken),
    "must be a data frame"
  )

  broken <- unclass(changes)
  expect_error(
    validate_network_changes(broken),
    "must inherit"
  )
})


test_that("validator detects inconsistent counts across object levels", {
  changes <- make_weighted_changes()

  broken <- changes
  broken$comparison$n_links_union <-
    broken$comparison$n_links_union + 1
  expect_error(
    validate_network_changes(broken),
    "n_links_union"
  )

  broken <- changes
  broken$comparison$n_nodes_union <-
    broken$comparison$n_nodes_union + 1
  expect_error(
    validate_network_changes(broken),
    "n_nodes_union"
  )

  broken <- changes
  broken$structural$n_links[1] <-
    broken$structural$n_links[1] + 1L
  expect_error(
    validate_network_changes(broken),
    "structural link counts"
  )

  broken <- changes
  broken$comparison$n_changed_links <-
    broken$comparison$n_changed_links + 1
  expect_error(
    validate_network_changes(broken),
    "n_changed_links"
  )
})


test_that("print.network_changes returns invisibly and prints key metadata", {
  changes <- make_weighted_changes()

  output <- capture.output(returned <- print(changes))

  expect_identical(returned, changes)
  expect_true(any(grepl("<network_changes>", output, fixed = TRUE)))
  expect_true(any(grepl("Comparison: temporal", output, fixed = TRUE)))
  expect_true(any(grepl("Changed:", output, fixed = TRUE)))
  expect_true(any(grepl("Use `summary()`", output, fixed = TRUE)))
})


test_that("summary.network_changes returns the expected summary object", {
  changes <- make_weighted_changes()
  result <- summary(changes)

  expect_s3_class(result, "summary.network_changes")
  expect_named(result, c("comparison", "structural"))
  expect_equal(result$comparison, changes$comparison)
  expect_equal(result$structural, changes$structural)
})


test_that("print.summary.network_changes returns invisibly and prints structural table", {
  changes <- make_weighted_changes()
  summary_object <- summary(changes)

  output <- capture.output(returned <- print(summary_object))

  expect_identical(returned, summary_object)
  expect_true(any(grepl("Summary of network changes", output, fixed = TRUE)))
  expect_true(any(grepl("Structural changes", output, fixed = TRUE)))
  expect_true(any(grepl("persistent", output, fixed = TRUE)))
})


test_that("weighted and binary objects both pass class validation", {
  expect_valid_network_changes(make_weighted_changes())
  expect_valid_network_changes(make_binary_changes())
})
