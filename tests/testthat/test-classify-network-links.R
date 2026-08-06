# =============================================================================
# Tests for classify_network_links()
# =============================================================================

test_that("links cover the union and all seven structural categories", {
  links <- classify_network_links(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  expect_s3_class(links, "data.frame")
  expect_equal(nrow(links), 8L)
  expect_equal(anyDuplicated(link_key(links)), 0L)
  expect_equal(unique(links$Comparison_type), "temporal")

  categories <- paste(links$Direction, links$Mechanism, sep = " / ")

  expect_equal(sum(categories == "persistent / permanent"), 2L)
  expect_equal(sum(categories == "lost / rewiring"), 1L)
  expect_equal(sum(categories == "lost / one_non_permanent_node"), 1L)
  expect_equal(sum(categories == "lost / two_non_permanent_nodes"), 1L)
  expect_equal(sum(categories == "gained / rewiring"), 1L)
  expect_equal(sum(categories == "gained / one_non_permanent_node"), 1L)
  expect_equal(sum(categories == "gained / two_non_permanent_nodes"), 1L)
})


test_that("presence, strength, and direction are coherent at link level", {
  links <- classify_network_links(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  expect_true(all(links$Present_network_1 | links$Present_network_2))

  expected_direction <- ifelse(
    links$Present_network_1 & links$Present_network_2,
    "persistent",
    ifelse(links$Present_network_1, "lost", "gained")
  )
  expect_identical(links$Direction, expected_direction)

  expect_true(all(
    links$Strength_network_1[!links$Present_network_1] == 0
  ))
  expect_true(all(
    links$Strength_network_2[!links$Present_network_2] == 0
  ))

  expect_true(all(
    links$Direction == "persistent" |
      links$Strength_network_1 == 0 |
      links$Strength_network_2 == 0
  ))
})


test_that("link node statuses agree with classify_network_nodes", {
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

  lookup <- stats::setNames(nodes$Node_status, nodes$Node)

  expect_identical(
    links$Origin_status,
    unname(lookup[links$Origin_node])
  )
  expect_identical(
    links$Destiny_status,
    unname(lookup[links$Destiny_node])
  )
})


test_that("mechanism is derived from direction and node statuses", {
  links <- classify_network_links(
    make_weighted_prepared(),
    "site_1", "early",
    "site_1", "late"
  )

  n_permanent <- 
    (links$Origin_status == "permanent") +
    (links$Destiny_status == "permanent")

  expected <- ifelse(
    links$Direction == "persistent",
    "permanent",
    ifelse(
      n_permanent == 2L,
      "rewiring",
      ifelse(
        n_permanent == 1L,
        "one_non_permanent_node",
        "two_non_permanent_nodes"
      )
    )
  )

  expect_identical(links$Mechanism, expected)
})


test_that("reversing comparison swaps lost and gained links", {
  prepared <- make_weighted_prepared()

  forward <- classify_network_links(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
  reverse <- classify_network_links(
    prepared,
    "site_1", "late",
    "site_1", "early"
  )

  reverse_index <- match(link_key(forward), link_key(reverse))

  expected_direction <- ifelse(
    forward$Direction == "lost",
    "gained",
    ifelse(
      forward$Direction == "gained",
      "lost",
      "persistent"
    )
  )

  expect_identical(
    reverse$Direction[reverse_index],
    expected_direction
  )
  expect_identical(
    reverse$Mechanism[reverse_index],
    forward$Mechanism
  )
  expect_equal(
    reverse$Strength_network_1[reverse_index],
    forward$Strength_network_2
  )
  expect_equal(
    reverse$Strength_network_2[reverse_index],
    forward$Strength_network_1
  )
})


test_that("invalid node classifications supplied to link classification fail", {
  prepared <- make_weighted_prepared()
  nodes <- classify_network_nodes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )

  broken <- nodes
  broken$Node_status[broken$Node == "bee_2"] <- "gained"

  expect_error(
    classify_network_links(
      prepared,
      "site_1", "early",
      "site_1", "late",
      node_status = broken
    )
  )
})
