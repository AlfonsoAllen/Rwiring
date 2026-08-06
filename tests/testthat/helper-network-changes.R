# =============================================================================
# Shared synthetic data and helpers for network-change tests
# =============================================================================

make_network_change_data <- function(weighted = TRUE) {
  data <- data.frame(
    Site = rep("site_1", 10),
    Time = c(rep("early", 5), rep("late", 5)),
    Origin_node = c(
      # Network 1
      "plant_1",
      "plant_2",
      "plant_1",
      "plant_1",
      "plant_lost",
      # Network 2
      "plant_1",
      "plant_2",
      "plant_2",
      "plant_1",
      "plant_gained"
    ),
    Destiny_node = c(
      # Network 1
      "bee_1",
      "bee_2",
      "bee_2",
      "bee_lost_1",
      "bee_lost_2",
      # Network 2
      "bee_1",
      "bee_2",
      "bee_1",
      "bee_gained_1",
      "bee_gained_2"
    ),
    Origin_guild = rep("plant", 10),
    Destiny_guild = rep("pollinator", 10),
    Strength = c(
      # Network 1
      5, 4, 3, 6, 2,
      # Network 2
      8, 2, 7, 5, 4
    ),
    stringsAsFactors = FALSE
  )

  if (!weighted) {
    data$Strength <- NULL
  }

  data
}


make_spatial_change_data <- function() {
  data <- make_network_change_data(weighted = TRUE)
  data$Site <- c(rep("site_1", 5), rep("site_2", 5))
  data$Time <- "survey"
  data
}


make_spatiotemporal_change_data <- function() {
  data <- make_network_change_data(weighted = TRUE)
  data$Site <- c(rep("site_1", 5), rep("site_2", 5))
  data$Time <- c(rep("early", 5), rep("late", 5))
  data
}


make_weighted_prepared <- function() {
  prepare_interactions(
    make_network_change_data(weighted = TRUE),
    directed = TRUE,
    multipartite = TRUE
  )
}


make_binary_prepared <- function() {
  prepare_interactions(
    make_network_change_data(weighted = FALSE),
    directed = TRUE,
    multipartite = TRUE
  )
}


make_weighted_changes <- function(strength_tolerance = 0) {
  quantify_network_changes(
    make_weighted_prepared(),
    site_1 = "site_1",
    time_1 = "early",
    site_2 = "site_1",
    time_2 = "late",
    strength_tolerance = strength_tolerance
  )
}


make_binary_changes <- function() {
  quantify_network_changes(
    make_binary_prepared(),
    site_1 = "site_1",
    time_1 = "early",
    site_2 = "site_1",
    time_2 = "late"
  )
}


get_structural_row <- function(structural, direction, mechanism) {
  structural[
    structural$Direction == direction &
      structural$Mechanism == mechanism,
    ,
    drop = FALSE
  ]
}


link_key <- function(x) {
  paste(
    as.character(x$Origin_node),
    as.character(x$Destiny_node),
    sep = " -> "
  )
}


expect_valid_network_changes <- function(x) {
  expect_s3_class(x, "network_changes")
  expect_invisible(validate_network_changes(x))
}
