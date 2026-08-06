# =============================================================================
# Classification of links between networks
# =============================================================================

#' Classify links between two interaction networks
#'
#' Compares the links present in two interaction networks and classifies each
#' link according to its persistence direction and the node-level mechanism
#' responsible for its presence, loss, or gain.
#'
#' Each network is identified by a unique site-time combination:
#'
#' * network 1: `site_1` and `time_1`;
#' * network 2: `site_2` and `time_2`.
#'
#' The comparison is directional and is interpreted as network 1 to network 2.
#'
#' @param x A `temporal_interactions` object created with
#'   [prepare_interactions()].
#' @param site_1 A single non-missing value identifying the site of network 1.
#' @param time_1 A single non-missing value identifying the time of network 1.
#' @param site_2 A single non-missing value identifying the site of network 2.
#' @param time_2 A single non-missing value identifying the time of network 2.
#' @param node_status Optional data frame returned by
#'   [classify_network_nodes()]. If `NULL`, node classifications are computed
#'   internally.
#' @param active_only Logical. If `TRUE`, only links with `Strength > 0` are
#'   treated as present. If `FALSE`, every selected row is retained, including
#'   zero-strength rows. Defaults to `TRUE`.
#'
#' @return A data frame with one row for each link occurring in at least one of
#'   the two networks. It contains:
#'
#' * network identifiers and comparison type;
#' * origin and destination node identifiers;
#' * guild information, when available;
#' * node status for both interaction partners;
#' * presence indicators for each network;
#' * interaction strength in each network;
#' * `Direction`: `"persistent"`, `"lost"`, or `"gained"`;
#' * `Mechanism`: `"permanent"`, `"rewiring"`,
#'   `"one_non_permanent_node"`, or `"two_non_permanent_nodes"`.
#'
#' @details
#' `Direction` describes the ordered comparison from network 1 to network 2:
#'
#' * `"persistent"`: the link is present in both networks;
#' * `"lost"`: the link is present only in network 1;
#' * `"gained"`: the link is present only in network 2.
#'
#' `Mechanism` describes the node-level composition of the change:
#'
#' * `"permanent"`: the link is persistent and both nodes are permanent;
#' * `"rewiring"`: a lost or gained link connects two permanent nodes;
#' * `"one_non_permanent_node"`: exactly one interaction partner is lost or
#'   gained;
#' * `"two_non_permanent_nodes"`: both interaction partners are lost or gained.
#'
#' The function does not interpret changes in interaction strength. It retains
#' `Strength_network_1` and `Strength_network_2` so that a later function can
#' quantify weighted changes when the original network is weighted.
#'
#' For binary networks, the stored strengths are presence values equal to one.
#'
#' @examples
#' interactions <- data.frame(
#'   Site = rep("site_1", 8),
#'   Time = c(rep(2020, 4), rep(2021, 4)),
#'   Origin_node = c(
#'     "plant_1", "plant_1", "plant_2", "plant_3",
#'     "plant_1", "plant_1", "plant_2", "plant_4"
#'   ),
#'   Destiny_node = c(
#'     "bee_1", "bee_2", "bee_3", "bee_4",
#'     "bee_1", "bee_3", "bee_5", "bee_6"
#'   ),
#'   Origin_guild = rep("plant", 8),
#'   Destiny_guild = rep("pollinator", 8),
#'   Strength = c(5, 3, 2, 4, 7, 6, 3, 1)
#' )
#'
#' prepared <- prepare_interactions(
#'   interactions,
#'   directed = FALSE,
#'   multipartite = TRUE
#' )
#'
#' classify_network_links(
#'   prepared,
#'   site_1 = "site_1",
#'   time_1 = 2020,
#'   site_2 = "site_1",
#'   time_2 = 2021
#' )
#'
#' @export
classify_network_links <- function(
    x,
    site_1,
    time_1,
    site_2,
    time_2,
    node_status = NULL,
    active_only = TRUE
) {
  validate_temporal_interactions(x)
  validate_flag(active_only, "active_only")
  
  validate_comparison_value <- function(value, argument) {
    if (
      length(value) != 1L ||
      anyNA(value)
    ) {
      stop(
        "`", argument, "` must be a single non-missing value.",
        call. = FALSE
      )
    }
  }
  
  validate_comparison_value(site_1, "site_1")
  validate_comparison_value(time_1, "time_1")
  validate_comparison_value(site_2, "site_2")
  validate_comparison_value(time_2, "time_2")
  
  same_site <- isTRUE(site_1 == site_2)
  same_time <- isTRUE(time_1 == time_2)
  
  if (same_site && same_time) {
    stop(
      "The two networks must correspond to different Site-Time combinations.",
      call. = FALSE
    )
  }
  
  data <- x$interactions
  
  network_1 <- data[
    data$Site == site_1 & data$Time == time_1,
    ,
    drop = FALSE
  ]
  
  network_2 <- data[
    data$Site == site_2 & data$Time == time_2,
    ,
    drop = FALSE
  ]
  
  if (nrow(network_1) == 0L) {
    stop(
      "No interaction data were found for `site_1` and `time_1`.",
      call. = FALSE
    )
  }
  
  if (nrow(network_2) == 0L) {
    stop(
      "No interaction data were found for `site_2` and `time_2`.",
      call. = FALSE
    )
  }
  
  if (active_only) {
    network_1 <- network_1[
      network_1$Strength > 0,
      ,
      drop = FALSE
    ]
    
    network_2 <- network_2[
      network_2$Strength > 0,
      ,
      drop = FALSE
    ]
    
    if (nrow(network_1) == 0L) {
      warning(
        "No active interactions remain in network 1.",
        call. = FALSE
      )
    }
    
    if (nrow(network_2) == 0L) {
      warning(
        "No active interactions remain in network 2.",
        call. = FALSE
      )
    }
  }
  
  if (
    nrow(network_1) == 0L &&
    nrow(network_2) == 0L
  ) {
    stop(
      "No active links remain in either network.",
      call. = FALSE
    )
  }
  
  if (is.null(node_status)) {
    node_status <- classify_network_nodes(
      x = x,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2,
      active_only = active_only
    )
  } else {
    validate_node_classification(
      node_status = node_status,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2
    )
  }
  
  link_keys <- c(
    "Origin_node",
    "Destiny_node"
  )
  
  has_guilds <- isTRUE(x$network_type$has_guilds)
  
  if (has_guilds) {
    link_keys <- c(
      link_keys,
      "Origin_guild",
      "Destiny_guild"
    )
  }
  
  links_1 <- network_1[
    ,
    c(link_keys, "Strength"),
    drop = FALSE
  ]
  
  links_2 <- network_2[
    ,
    c(link_keys, "Strength"),
    drop = FALSE
  ]
  
  names(links_1)[names(links_1) == "Strength"] <-
    "Strength_network_1"
  
  names(links_2)[names(links_2) == "Strength"] <-
    "Strength_network_2"
  
  links <- merge(
    links_1,
    links_2,
    by = link_keys,
    all = TRUE,
    sort = TRUE
  )
  
  links$Present_network_1 <-
    !is.na(links$Strength_network_1)
  
  links$Present_network_2 <-
    !is.na(links$Strength_network_2)
  
  links$Strength_network_1[
    is.na(links$Strength_network_1)
  ] <- 0
  
  links$Strength_network_2[
    is.na(links$Strength_network_2)
  ] <- 0
  
  node_lookup <- stats::setNames(
    node_status$Node_status,
    node_status$Node
  )
  
  links$Origin_status <- unname(
    node_lookup[as.character(links$Origin_node)]
  )
  
  links$Destiny_status <- unname(
    node_lookup[as.character(links$Destiny_node)]
  )
  
  if (
    anyNA(links$Origin_status) ||
    anyNA(links$Destiny_status)
  ) {
    stop(
      "Node classifications could not be matched to every interaction node.",
      call. = FALSE
    )
  }
  
  links$Direction <- ifelse(
    links$Present_network_1 &
      links$Present_network_2,
    "persistent",
    ifelse(
      links$Present_network_1,
      "lost",
      "gained"
    )
  )
  
  lost_links <- links$Direction == "lost"
  gained_links <- links$Direction == "gained"
  
  if (
    any(links$Origin_status[lost_links] == "gained") ||
    any(links$Destiny_status[lost_links] == "gained")
  ) {
    stop(
      "Lost links cannot contain nodes classified as gained.",
      call. = FALSE
    )
  }
  
  if (
    any(links$Origin_status[gained_links] == "lost") ||
    any(links$Destiny_status[gained_links] == "lost")
  ) {
    stop(
      "Gained links cannot contain nodes classified as lost.",
      call. = FALSE
    )
  }
  
  permanent_nodes <-
    (links$Origin_status == "permanent") +
    (links$Destiny_status == "permanent")
  
  links$Mechanism <- ifelse(
    links$Direction == "persistent",
    "permanent",
    ifelse(
      permanent_nodes == 2L,
      "rewiring",
      ifelse(
        permanent_nodes == 1L,
        "one_non_permanent_node",
        "two_non_permanent_nodes"
      )
    )
  )
  
  comparison_type <- if (same_site && !same_time) {
    "temporal"
  } else if (!same_site && same_time) {
    "spatial"
  } else {
    "spatiotemporal"
  }
  
  links$Site_1 <- site_1
  links$Time_1 <- time_1
  links$Site_2 <- site_2
  links$Time_2 <- time_2
  links$Comparison_type <- comparison_type
  
  first_columns <- c(
    "Site_1",
    "Time_1",
    "Site_2",
    "Time_2",
    "Comparison_type",
    "Origin_node",
    "Destiny_node"
  )
  
  if (has_guilds) {
    first_columns <- c(
      first_columns,
      "Origin_guild",
      "Destiny_guild"
    )
  }
  
  first_columns <- c(
    first_columns,
    "Origin_status",
    "Destiny_status",
    "Present_network_1",
    "Present_network_2",
    "Strength_network_1",
    "Strength_network_2",
    "Direction",
    "Mechanism"
  )
  
  links[first_columns]
}