# =============================================================================
# Classification of nodes between networks
# =============================================================================

#' Classify nodes between two interaction networks
#'
#' Compares the nodes present in two interaction networks and classifies each
#' node as permanent, lost, or gained in the ordered transition from network 1
#' to network 2.
#'
#' Each network is identified by a unique combination of site and time:
#'
#' * network 1: `site_1` and `time_1`;
#' * network 2: `site_2` and `time_2`.
#'
#' The function therefore supports temporal comparisons within a site, spatial
#' comparisons at a common time, and spatiotemporal comparisons between
#' different sites and times.
#'
#' @param x A `temporal_interactions` object created with
#'   [prepare_interactions()].
#' @param site_1 A single non-missing value identifying the site of network 1.
#' @param time_1 A single non-missing value identifying the time of network 1.
#' @param site_2 A single non-missing value identifying the site of network 2.
#' @param time_2 A single non-missing value identifying the time of network 2.
#' @param active_only Logical. If `TRUE`, only interactions with
#'   `Strength > 0` are used to determine node presence. If `FALSE`, every row
#'   in the selected networks is considered, including rows with
#'   `Strength = 0`. Defaults to `TRUE`.
#'
#' @return A data frame with one row per node occurring in at least one of the
#'   two networks. It contains:
#'
#' * `Site_1`: site identifying network 1;
#' * `Time_1`: time identifying network 1;
#' * `Site_2`: site identifying network 2;
#' * `Time_2`: time identifying network 2;
#' * `Comparison_type`: `"temporal"`, `"spatial"`, or `"spatiotemporal"`;
#' * `Node`: node identifier;
#' * `Guild`: guild assigned to the node, or `NA` if guild information was not
#'   supplied;
#' * `Present_network_1`: whether the node is present in network 1;
#' * `Present_network_2`: whether the node is present in network 2;
#' * `Node_status`: classification as `"permanent"`, `"lost"`, or `"gained"`.
#'
#' @details
#' Node presence is inferred from participation in at least one interaction in
#' the corresponding network. Nodes are extracted from both `Origin_node` and
#' `Destiny_node`.
#'
#' The classifications are defined relative to the ordered comparison from
#' network 1 to network 2:
#'
#' * `"permanent"`: the node occurs in both networks;
#' * `"lost"`: the node occurs in network 1 but not in network 2;
#' * `"gained"`: the node occurs in network 2 but not in network 1.
#'
#' These terms describe the direction of the comparison. In a temporal
#' comparison, they can be interpreted as persistence, loss, and gain through
#' time. In spatial or spatiotemporal comparisons, `"lost"` and `"gained"`
#' indicate that a node is exclusive to network 1 or network 2, respectively,
#' and do not necessarily imply a historical disappearance or colonization.
#'
#' `Comparison_type` is inferred automatically:
#'
#' * `"temporal"` when both networks belong to the same site but different
#'   times;
#' * `"spatial"` when both networks belong to different sites at the same time;
#' * `"spatiotemporal"` when both site and time differ.
#'
#' The two networks must correspond to distinct `Site`-`Time` combinations.
#' Both combinations must exist in the interaction data before filtering by
#' `active_only`.
#'
#' If filtering active interactions leaves either network empty, the function
#' issues a warning. All nodes in the non-empty network will then be classified
#' as lost or gained according to the comparison direction.
#'
#' @examples
#' interactions <- data.frame(
#'   Site = c(
#'     "site_1", "site_1", "site_1",
#'     "site_1", "site_1", "site_1"
#'   ),
#'   Time = c(2020, 2020, 2020, 2021, 2021, 2021),
#'   Origin_node = c(
#'     "plant_1", "plant_2", "plant_3",
#'     "plant_1", "plant_2", "plant_4"
#'   ),
#'   Destiny_node = c(
#'     "bee_1", "bee_2", "bee_3",
#'     "bee_1", "bee_4", "bee_5"
#'   ),
#'   Origin_guild = rep("plant", 6),
#'   Destiny_guild = rep("pollinator", 6),
#'   Strength = c(5, 3, 2, 7, 4, 1)
#' )
#'
#' prepared <- prepare_interactions(
#'   interactions,
#'   directed = FALSE,
#'   multipartite = TRUE
#' )
#'
#' classify_network_nodes(
#'   prepared,
#'   site_1 = "site_1",
#'   time_1 = 2020,
#'   site_2 = "site_1",
#'   time_2 = 2021
#' )
#'
#' @export
classify_network_nodes <- function(
    x,
    site_1,
    time_1,
    site_2,
    time_2,
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
  
  nodes_1 <- unique(c(
    as.character(network_1$Origin_node),
    as.character(network_1$Destiny_node)
  ))
  
  nodes_2 <- unique(c(
    as.character(network_2$Origin_node),
    as.character(network_2$Destiny_node)
  ))
  
  all_nodes <- sort(union(nodes_1, nodes_2))
  
  if (length(all_nodes) == 0L) {
    stop(
      "No active nodes remain in either network after applying ",
      "`active_only = TRUE`.",
      call. = FALSE
    )
  }
  
  present_1 <- all_nodes %in% nodes_1
  present_2 <- all_nodes %in% nodes_2
  
  node_status <- ifelse(
    present_1 & present_2,
    "permanent",
    ifelse(
      present_1,
      "lost",
      "gained"
    )
  )
  
  guild <- rep(NA_character_, length(all_nodes))
  
  if (isTRUE(x$network_type$has_guilds)) {
    guild_data <- rbind(
      data.frame(
        Node = as.character(data$Origin_node),
        Guild = as.character(data$Origin_guild),
        stringsAsFactors = FALSE
      ),
      data.frame(
        Node = as.character(data$Destiny_node),
        Guild = as.character(data$Destiny_guild),
        stringsAsFactors = FALSE
      )
    )
    
    guild_data <- unique(guild_data)
    
    guild <- guild_data$Guild[
      match(all_nodes, guild_data$Node)
    ]
  }
  
  comparison_type <- if (same_site && !same_time) {
    "temporal"
  } else if (!same_site && same_time) {
    "spatial"
  } else {
    "spatiotemporal"
  }
  
  data.frame(
    Site_1 = rep(site_1, length(all_nodes)),
    Time_1 = rep(time_1, length(all_nodes)),
    Site_2 = rep(site_2, length(all_nodes)),
    Time_2 = rep(time_2, length(all_nodes)),
    Comparison_type = rep(comparison_type, length(all_nodes)),
    Node = all_nodes,
    Guild = guild,
    Present_network_1 = present_1,
    Present_network_2 = present_2,
    Node_status = node_status,
    stringsAsFactors = FALSE
  )
}