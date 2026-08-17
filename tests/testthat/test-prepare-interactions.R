# Tests for prepare_interactions()
# Place this file in tests/testthat/test-prepare-interactions.R

library(testthat)

make_weighted_data <- function() {
  data.frame(
    Site = c("site_1", "site_1", "site_1", "site_1"),
    Time = c(1, 1, 2, 2),
    Origin_node = c("plant_1", "plant_2", "plant_1", "plant_2"),
    Destiny_node = c("bee_1", "bee_2", "bee_1", "bee_2"),
    Origin_guild = c("plant", "plant", "plant", "plant"),
    Destiny_guild = c("pollinator", "pollinator", "pollinator", "pollinator"),
    Strength = c(2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
}

test_that("valid weighted multipartite data are prepared", {
  x <- prepare_interactions(make_weighted_data())

  expect_s3_class(x, "temporal_interactions")
  expect_true(x$network_type$directed)
  expect_identical(x$network_type$topology, "multipartite")
  expect_true(x$network_type$weighted)
  expect_false(x$network_type$allow_self_links)
  expect_true(x$network_type$has_guilds)
  expect_identical(x$network_type$n_guilds, 2L)
  expect_equal(x$network_type$guilds, c("plant", "pollinator"))
  expect_equal(nrow(x$interactions), 4L)
})

test_that("data without guild columns are treated as unipartite", {
  dat <- make_weighted_data()[, c(
    "Site", "Time", "Origin_node", "Destiny_node", "Strength"
  )]

  x <- prepare_interactions(dat)

  expect_false(x$network_type$has_guilds)
  expect_identical(x$network_type$topology, "unipartite")
  expect_true(is.na(x$network_type$n_guilds))
  expect_null(x$network_type$guilds)
})

test_that("one explicit guild is treated as unipartite", {
  dat <- make_weighted_data()
  dat$Origin_guild <- "organism"
  dat$Destiny_guild <- "organism"

  x <- prepare_interactions(dat)

  expect_true(x$network_type$has_guilds)
  expect_identical(x$network_type$topology, "unipartite")
  expect_identical(x$network_type$n_guilds, 1L)
  expect_equal(x$network_type$guilds, "organism")
})

test_that("more than two guilds can define a multipartite network", {
  dat <- data.frame(
    Site = "site_1",
    Time = 1,
    Origin_node = c("plant_1", "bee_1", "plant_1"),
    Destiny_node = c("bee_1", "parasitoid_1", "parasitoid_1"),
    Origin_guild = c("plant", "pollinator", "plant"),
    Destiny_guild = c("pollinator", "parasitoid", "parasitoid"),
    Strength = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  x <- prepare_interactions(dat)

  expect_identical(x$network_type$topology, "multipartite")
  expect_identical(x$network_type$n_guilds, 3L)
})

test_that("multipartite TRUE requires guild information", {
  dat <- make_weighted_data()[, c(
    "Site", "Time", "Origin_node", "Destiny_node", "Strength"
  )]

  expect_error(
    prepare_interactions(dat, multipartite = TRUE),
    "requires guild information"
  )
})

test_that("both guild columns must be supplied", {
  dat <- make_weighted_data()
  dat$Destiny_guild <- NULL

  expect_error(
    prepare_interactions(dat),
    "Both `Origin_guild` and `Destiny_guild`"
  )
})

test_that("partially missing guild information is rejected", {
  dat <- make_weighted_data()
  dat$Origin_guild[1] <- NA

  expect_error(
    prepare_interactions(dat),
    "Guild information is partially missing"
  )
})

test_that("a node cannot belong to multiple guilds", {
  dat <- make_weighted_data()
  dat$Origin_guild[3] <- "pollinator"

  expect_error(
    prepare_interactions(dat),
    "assigned to more than one guild"
  )
})

test_that("multipartite TRUE rejects within-guild interactions", {
  dat <- make_weighted_data()
  dat$Destiny_guild[1] <- "plant"
  dat$Destiny_node[1] <- "plant_3"

  expect_error(
    prepare_interactions(dat, multipartite = TRUE),
    "do not satisfy a multipartite structure"
  )
})

test_that("factor guild columns with different levels are supported", {
  dat <- make_weighted_data()
  dat$Origin_guild <- factor(dat$Origin_guild, levels = "plant")
  dat$Destiny_guild <- factor(dat$Destiny_guild, levels = "pollinator")

  expect_no_error(x <- prepare_interactions(dat))
  expect_identical(x$network_type$topology, "multipartite")
})

test_that("missing Strength column creates a binary network", {
  dat <- make_weighted_data()
  dat$Strength <- NULL

  x <- prepare_interactions(dat)

  expect_false(x$network_type$weighted)
  expect_true(all(x$interactions$Strength == 1))
  expect_identical(x$diagnostics$binary_conversion, "missing_column")
})

test_that("entirely missing Strength creates a binary network", {
  dat <- make_weighted_data()
  dat$Strength <- NA_real_

  x <- prepare_interactions(dat)

  expect_false(x$network_type$weighted)
  expect_true(all(x$interactions$Strength == 1))
  expect_identical(x$diagnostics$binary_conversion, "all_missing")
})

test_that("partially missing Strength errors by default", {
  dat <- make_weighted_data()
  dat$Strength[1] <- NA_real_

  expect_error(
    prepare_interactions(dat),
    "`Strength` is partially missing"
  )
})

test_that("partially missing Strength can be converted to binary", {
  dat <- make_weighted_data()
  dat$Strength[1] <- NA_real_

  x <- prepare_interactions(dat, missing_strength = "binary")

  expect_false(x$network_type$weighted)
  expect_true(all(x$interactions$Strength == 1))
  expect_identical(x$diagnostics$binary_conversion, "partial_missing")
})

test_that("warn_binary warns and converts all strengths to one", {
  dat <- make_weighted_data()
  dat$Strength[1] <- NA_real_

  expect_warning(
    x <- prepare_interactions(dat, missing_strength = "warn_binary"),
    "Existing weights will be discarded"
  )
  expect_false(x$network_type$weighted)
  expect_true(all(x$interactions$Strength == 1))
})

test_that("non-numeric Strength is rejected", {
  dat <- make_weighted_data()
  dat$Strength <- as.character(dat$Strength)

  expect_error(prepare_interactions(dat), "must be numeric")
})

test_that("non-finite Strength is rejected", {
  dat <- make_weighted_data()
  dat$Strength[1] <- Inf

  expect_error(prepare_interactions(dat), "finite values")
})

test_that("negative Strength is rejected", {
  dat <- make_weighted_data()
  dat$Strength[1] <- -1

  expect_error(prepare_interactions(dat), "cannot contain negative")
})

test_that("zero strengths are removed with diagnostics", {
  dat <- make_weighted_data()
  dat$Strength[1] <- 0

  expect_warning(
    x <- prepare_interactions(dat, zero_strength = "remove"),
    "were removed"
  )
  expect_equal(nrow(x$interactions), 3L)
  expect_identical(x$diagnostics$zero_strength_rows_removed, 1L)
})

test_that("zero strengths can be retained", {
  dat <- make_weighted_data()
  dat$Strength[1] <- 0

  expect_warning(
    x <- prepare_interactions(dat, zero_strength = "keep"),
    "were retained"
  )
  expect_equal(nrow(x$interactions), 4L)
  expect_true(any(x$interactions$Strength == 0))
})

test_that("zero strengths can trigger an error", {
  dat <- make_weighted_data()
  dat$Strength[1] <- 0

  expect_error(
    prepare_interactions(dat, zero_strength = "error"),
    "zero values"
  )
})

test_that("removing all zero-strength rows triggers an error", {
  dat <- make_weighted_data()
  dat$Strength <- 0

  expect_error(
    suppressWarnings(prepare_interactions(dat, zero_strength = "remove")),
    "no active links remain"
  )
})

test_that("self-links are rejected by default", {
  dat <- make_weighted_data()[1, ]
  dat$Origin_node <- "node_1"
  dat$Destiny_node <- "node_1"
  dat$Origin_guild <- "organism"
  dat$Destiny_guild <- "organism"

  expect_error(
    prepare_interactions(dat),
    "Self-links were found"
  )
})

test_that("self-links can be allowed in unipartite networks", {
  dat <- make_weighted_data()[1, ]
  dat$Origin_node <- "node_1"
  dat$Destiny_node <- "node_1"
  dat$Origin_guild <- "organism"
  dat$Destiny_guild <- "organism"

  expect_no_error(
    x <- prepare_interactions(dat, allow_self_links = TRUE)
  )
  expect_equal(nrow(x$interactions), 1L)
  expect_true(x$network_type$allow_self_links)
})

test_that("self-links remain invalid in multipartite networks", {
  dat <- make_weighted_data()[1, ]
  dat$Origin_node <- "node_1"
  dat$Destiny_node <- "node_1"
  dat$Origin_guild <- "plant"
  dat$Destiny_guild <- "pollinator"

  # This dataset is already invalid because the same node has two guilds.
  expect_error(
    prepare_interactions(
      dat,
      multipartite = TRUE,
      allow_self_links = TRUE
    ),
    "assigned to more than one guild|Self-links are not allowed"
  )
})

test_that("self-link permission is stored in network metadata", {
  dat <- make_weighted_data()[1, c(
    "Site", "Time", "Origin_node", "Destiny_node", "Strength"
  )]
  
  x <- prepare_interactions(
    dat,
    multipartite = FALSE
  )
  
  expect_false(x$network_type$allow_self_links)
  
  y <- prepare_interactions(
    dat,
    multipartite = FALSE,
    allow_self_links = TRUE
  )
  
  expect_true(y$network_type$allow_self_links)
})

test_that("multipartite networks store self-link permission as false", {
  x <- prepare_interactions(
    make_weighted_data(),
    multipartite = TRUE,
    allow_self_links = TRUE
  )
  
  expect_false(x$network_type$allow_self_links)
})

test_that("undirected links are canonicalized", {
  dat <- data.frame(
    Site = "site_1",
    Time = 1,
    Origin_node = "bee_1",
    Destiny_node = "plant_1",
    Origin_guild = "pollinator",
    Destiny_guild = "plant",
    Strength = 2,
    stringsAsFactors = FALSE
  )

  x <- prepare_interactions(dat, directed = FALSE)

  expect_equal(x$interactions$Origin_node, "bee_1")
  expect_equal(x$interactions$Destiny_node, "plant_1")
  expect_equal(x$interactions$Origin_guild, "pollinator")
  expect_equal(x$interactions$Destiny_guild, "plant")
})

test_that("reversed undirected links are aggregated", {
  dat <- data.frame(
    Site = c("site_1", "site_1"),
    Time = c(1, 1),
    Origin_node = c("plant_1", "bee_1"),
    Destiny_node = c("bee_1", "plant_1"),
    Origin_guild = c("plant", "pollinator"),
    Destiny_guild = c("pollinator", "plant"),
    Strength = c(2, 3),
    stringsAsFactors = FALSE
  )

  x <- prepare_interactions(dat, directed = FALSE)

  expect_equal(nrow(x$interactions), 1L)
  expect_equal(x$interactions$Strength, 5)
  expect_identical(x$diagnostics$duplicate_rows_aggregated, 1L)
})

test_that("weighted duplicate links are summed", {
  dat <- rbind(make_weighted_data()[1, ], make_weighted_data()[1, ])
  dat$Strength <- c(2, 3)

  x <- prepare_interactions(dat, duplicate_links = "aggregate")

  expect_equal(nrow(x$interactions), 1L)
  expect_equal(x$interactions$Strength, 5)
})

test_that("binary duplicate links collapse to one presence", {
  dat <- rbind(make_weighted_data()[1, ], make_weighted_data()[1, ])
  dat$Strength <- NULL

  x <- prepare_interactions(dat, duplicate_links = "aggregate")

  expect_equal(nrow(x$interactions), 1L)
  expect_equal(x$interactions$Strength, 1)
  expect_false(x$network_type$weighted)
})

test_that("duplicate links can trigger an error", {
  dat <- rbind(make_weighted_data()[1, ], make_weighted_data()[1, ])

  expect_error(
    prepare_interactions(dat, duplicate_links = "error"),
    "Duplicated links"
  )
})

test_that("additional columns are removed consistently", {
  dat <- make_weighted_data()
  dat$Observer <- "A"

  x <- prepare_interactions(dat)

  expect_false("Observer" %in% names(x$interactions))
})

test_that("invalid input data are rejected", {
  expect_error(prepare_interactions(list()), "must be a data frame")
  expect_error(prepare_interactions(data.frame()), "contains no rows")
})

test_that("missing required columns are rejected", {
  dat <- make_weighted_data()
  dat$Site <- NULL

  expect_error(prepare_interactions(dat), "Missing required columns")
})

test_that("missing and empty identifiers are rejected", {
  dat_na <- make_weighted_data()
  dat_na$Origin_node[1] <- NA

  dat_empty <- make_weighted_data()
  dat_empty$Destiny_node[1] <- "   "

  expect_error(prepare_interactions(dat_na), "missing or empty")
  expect_error(prepare_interactions(dat_empty), "missing or empty")
})

test_that("logical flags must be scalar and non-missing", {
  dat <- make_weighted_data()

  expect_error(prepare_interactions(dat, directed = NA), "single non-missing logical")
  expect_error(prepare_interactions(dat, directed = c(TRUE, FALSE)), "single non-missing logical")
  expect_error(prepare_interactions(dat, multipartite = "yes"), "single non-missing logical")
  expect_error(prepare_interactions(dat, allow_self_links = 1), "single non-missing logical")
})

test_that("print displays alphabetically ordered guild and node names", {
  x <- prepare_interactions(make_weighted_data())
  output <- capture.output(print(x))
  output <- paste(output, collapse = "\n")

  expect_match(output, "Guild names \\(2\\)")
  expect_match(output, '"plant", "pollinator"', fixed = TRUE)
  expect_match(output, "Node names \\(4\\)")
  expect_match(
    output,
    '"bee_1", "bee_2", "plant_1", "plant_2"',
    fixed = TRUE
  )
})

test_that("summary and as.data.frame methods are coherent", {
  x <- prepare_interactions(make_weighted_data())
  s <- summary(x)

  expect_s3_class(s, "summary.temporal_interactions")
  expect_equal(s$n_rows, 4L)
  expect_equal(s$n_sites, 1L)
  expect_equal(s$n_times, 2L)
  expect_equal(s$n_networks, 2L)
  expect_equal(s$n_nodes, 4L)
  expect_equal(s$n_link_records, 4L)
  expect_equal(s$n_unique_links, 2L)
  expect_equal(s$total_strength, 14)
  expect_equal(as.data.frame(x), x$interactions)
  expect_false(s$allow_self_links)

  summary_output <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(summary_output, "Recorded links:  4", fixed = TRUE)
  expect_match(summary_output, "Unique links:    2", fixed = TRUE)
  expect_false(grepl("Node names", summary_output, fixed = TRUE))
  expect_match(
    summary_output,
    "Self-links:      not allowed",
    fixed = TRUE
  )
})

