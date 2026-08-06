# =============================================================================
# Tests for validate_link_classification()
# =============================================================================

valid_links_for_tests <- function() {
  classify_network_links(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )
}


test_that("a link classification created internally is valid", {
  links <- valid_links_for_tests()

  expect_invisible(
    validate_link_classification(
      links,
      "site_1", "early",
      "site_1", "late"
    )
  )
})


test_that("link table type, rows, and columns are validated", {
  expect_error(
    validate_link_classification(
      list(),
      "site_1", "early",
      "site_1", "late"
    ),
    "must be a data frame"
  )

  expect_error(
    validate_link_classification(
      data.frame(),
      "site_1", "early",
      "site_1", "late"
    ),
    "at least one link"
  )

  links <- valid_links_for_tests()
  links$Direction <- NULL

  expect_error(
    validate_link_classification(
      links,
      "site_1", "early",
      "site_1", "late"
    ),
    "missing required columns"
  )
})


test_that("link identifiers, uniqueness, and paired guild columns are validated", {
  links <- valid_links_for_tests()

  broken <- links
  broken$Origin_node[1] <- ""
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "missing or empty"
  )

  broken <- rbind(links, links[1, , drop = FALSE])
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "one row per link"
  )

  broken <- links
  broken$Destiny_guild <- NULL
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "must be supplied together"
  )
})


test_that("presence and strengths are validated", {
  links <- valid_links_for_tests()

  broken <- links
  broken$Present_network_1 <- as.integer(broken$Present_network_1)
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "non-missing logical"
  )

  broken <- links
  broken$Present_network_1[1] <- FALSE
  broken$Present_network_2[1] <- FALSE
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "at least one network"
  )

  broken <- links
  broken$Strength_network_1[1] <- -1
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "finite, non-negative"
  )

  broken <- links
  row <- which(!broken$Present_network_1)[1]
  broken$Strength_network_1[row] <- 1
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "absent link must have strength zero"
  )
})


test_that("direction must match link presence", {
  links <- valid_links_for_tests()
  broken <- links
  broken$Direction[1] <- if (
    broken$Direction[1] == "persistent"
  ) "lost" else "persistent"

  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "Direction.*presence indicators"
  )
})


test_that("node statuses must be valid for each direction", {
  links <- valid_links_for_tests()

  persistent <- which(links$Direction == "persistent")[1]
  broken <- links
  broken$Origin_status[persistent] <- "lost"
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "Persistent links must connect"
  )

  lost <- which(links$Direction == "lost")[1]
  broken <- links
  broken$Origin_status[lost] <- "gained"
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "Lost links cannot contain"
  )

  gained <- which(links$Direction == "gained")[1]
  broken <- links
  broken$Origin_status[gained] <- "lost"
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "Gained links cannot contain"
  )
})


test_that("mechanism must match direction and node statuses", {
  links <- valid_links_for_tests()
  broken <- links

  row <- which(
    broken$Direction == "lost" &
      broken$Mechanism == "rewiring"
  )[1]
  broken$Mechanism[row] <- "one_non_permanent_node"

  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "Mechanism.*inconsistent"
  )
})


test_that("comparison identifiers and comparison type are validated", {
  links <- valid_links_for_tests()

  expect_error(
    validate_link_classification(
      links,
      "site_1", "late",
      "site_1", "early"
    ),
    "does not correspond"
  )

  broken <- links
  broken$Comparison_type <- "spatial"
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "inconsistent with the requested comparison"
  )

  broken <- links
  broken$Comparison_type <- "invalid"
  expect_error(
    validate_link_classification(
      broken,
      "site_1", "early",
      "site_1", "late"
    ),
    "contains invalid values"
  )
})
