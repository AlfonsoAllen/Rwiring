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
        changes$links$Mechanism == row$Mechanism
    )

    expect_equal(row$n_links, observed)
  }

  expect_equal(
    sum(changes$structural$n_links),
    nrow(changes$links)
  )
})


test_that("all seven structural categories are represented in fixed order", {
  structural <- make_weighted_changes()$structural

  expect_equal(nrow(structural), 7L)

  expect_identical(
    paste(structural$Direction, structural$Mechanism),
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

  expect_identical(
    structural$n_links,
    c(2L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
})


test_that("structural proportions use the intended denominators", {
  structural <- make_weighted_changes()$structural

  expect_equal(
    sum(structural$proportion_all_links),
    1,
    tolerance = 1e-12
  )

  for (direction in unique(structural$Direction)) {
    selected <- structural$Direction == direction
    if (sum(structural$n_links[selected]) > 0L) {
      expect_equal(
        sum(
          structural$proportion_within_direction[selected],
          na.rm = TRUE
        ),
        1,
        tolerance = 1e-12
      )
    }
  }

  persistent <- structural$Direction == "persistent"
  changed <- !persistent

  expect_true(all(is.na(
    structural$proportion_changed_links[persistent]
  )))
  expect_equal(
    sum(
      structural$proportion_changed_links[changed],
      na.rm = TRUE
    ),
    1,
    tolerance = 1e-12
  )

  lost_rewiring <- get_structural_row(
    structural,
    "lost",
    "rewiring"
  )
  expect_equal(lost_rewiring$proportion_all_links, 1 / 8)
  expect_equal(lost_rewiring$proportion_within_direction, 1 / 3)
  expect_equal(lost_rewiring$proportion_changed_links, 1 / 6)
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
  expect_equal(
    changes$comparison$n_changed_links,
    sum(changes$links$Direction %in% c("lost", "gained"))
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
      changes$links$Mechanism == row$Mechanism

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
  structural <- make_weighted_changes()$structural

  persistent <- get_structural_row(
    structural,
    "persistent",
    "permanent"
  )
  expect_equal(persistent$total_strength_network_1, 9)
  expect_equal(persistent$total_strength_network_2, 10)
  expect_equal(persistent$net_strength_change, 1)
  expect_equal(persistent$total_absolute_strength_change, 5)
  expect_equal(persistent$mean_strength_change, 0.5)
  expect_equal(persistent$mean_absolute_strength_change, 2.5)

  lost_rewiring <- get_structural_row(
    structural,
    "lost",
    "rewiring"
  )
  expect_equal(lost_rewiring$net_strength_change, -3)

  gained_rewiring <- get_structural_row(
    structural,
    "gained",
    "rewiring"
  )
  expect_equal(gained_rewiring$net_strength_change, 7)
})


test_that("binary results omit weighted change columns", {
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
    "Absolute_strength_change",
    "Strength_direction"
  )

  expect_false(any(
    weighted_structural_columns %in% names(binary$structural)
  ))
  expect_false(any(
    weighted_link_columns %in% names(binary$links)
  ))
})


test_that("binary and weighted inputs have the same structural classification", {
  weighted <- make_weighted_changes()
  binary <- make_binary_changes()

  expect_equal(
    binary$structural[
      c("Direction", "Mechanism", "n_links")
    ],
    weighted$structural[
      c("Direction", "Mechanism", "n_links")
    ]
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
