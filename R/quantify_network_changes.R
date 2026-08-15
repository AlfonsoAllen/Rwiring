# =============================================================================
# Quantification of changes between interaction networks
# =============================================================================

#' Quantify changes between two interaction networks
#'
#' Compares two interaction networks and summarizes changes in their
#' interaction structure. For weighted networks, changes in interaction
#' strength are additionally incorporated into the classification and
#' summarized using aggregated and mean strength metrics.
#'
#' The comparison is directional and is interpreted as the transition from
#' network 1 to network 2. Each network is identified by a site-time
#' combination.
#'
#' @param x A `temporal_interactions` object created with
#'   [prepare_interactions()].
#' @param site_1 A single non-missing value identifying the site of network 1.
#' @param time_1 A single non-missing value identifying the time of network 1.
#' @param site_2 A single non-missing value identifying the site of network 2.
#' @param time_2 A single non-missing value identifying the time of network 2.
#' @param node_classification An optional data frame returned by
#'   [classify_network_nodes()]. If supplied, the table is validated for
#'   structural consistency and checked against `link_classification`.
#'   If `NULL`, node classifications are computed internally.
#' @param link_classification An optional data frame returned by
#'   [classify_network_links()]. If supplied, the table is validated for
#'   structural consistency and checked against `node_classification`.
#'   If `NULL`, link classifications are computed internally.
#' @param active_only Logical. If `TRUE`, only interactions with
#'   `Strength > 0` are considered present. Defaults to `TRUE`.
#' @param strength_tolerance A single finite non-negative numeric value used
#'   to distinguish unchanged, strengthened, and weakened persistent links
#'   in weighted networks. Gained and lost links are always classified as
#'   strengthened and weakened, respectively. Defaults to zero.
#'
#' @return An object of class `network_changes`, created by
#' `new_network_changes()`. The object contains four components:
#'
#' * `comparison`: metadata describing the network comparison, including the
#'   site and time identifiers, comparison type, network properties, and the
#'   numbers of nodes and links in each network and in their union.
#' * `structural`: link counts for each valid combination of `Direction`,
#'   `Mechanism`, and `Strength_direction`, complemented with aggregated
#'   and mean strength metrics for weighted networks;
#' * `links`: link-level classifications for every interaction occurring in
#'   at least one network. The table includes link presence, interaction
#'   strength in each network, `Direction`, `Mechanism`, and
#'   `Strength_direction`. For weighted networks, it additionally contains
#'   `Strength_change` and `Absolute_strength_change`;
#' * `nodes`: node-level classifications.
#'
#' @details
#' Link changes are classified using three complementary dimensions:
#'
#' * `Direction`: `"persistent"`, `"lost"`, or `"gained"`;
#' * `Mechanism`: `"permanent"`, `"rewiring"`,
#'   `"one_non_permanent_node"`, or `"two_non_permanent_nodes"`;
#' * `Strength_direction`: `"unchanged"`, `"weakened"`, or
#'   `"strengthened"`.
#'
#' When precomputed node or link classifications are supplied, the function
#' verifies that both tables describe the same network comparison and are
#' mutually consistent before computing any summary statistics.
#'
#' For weighted networks, the following columns are added to the
#' `structural` table:
#'
#' * `total_strength_network_1`: sum of interaction strengths in network 1;
#' * `total_strength_network_2`: sum of interaction strengths in network 2;
#' * `net_strength_change`: sum of signed strength changes;
#' * `total_absolute_strength_change`: sum of absolute strength changes;
#' * `mean_strength_change`: mean signed change per link;
#' * `mean_absolute_strength_change`: mean absolute change per link.
#'
#' Aggregate values are zero for link-change categories containing no links,
#' whereas mean values are `NA`.
#'
#' All valid combinations of `Direction`, `Mechanism`, and
#' `Strength_direction` are included in the `structural` table, even when
#' no links belong to a given category.
#'
#' For binary networks, seven combinations are possible. Persistent links
#' are necessarily classified as `"unchanged"`, lost links as `"weakened"`,
#' and gained links as `"strengthened"`.
#'
#' For weighted networks, persistent links may additionally be classified as
#' `"weakened"` or `"strengthened"` according to their change in interaction
#' strength. Consequently, weighted networks contain nine valid categories.
#'
#' `Strength_direction` is classified as:
#'
#' * `"strengthened"` for gained links and for persistent links whose strength
#'   increases by more than `strength_tolerance`;
#' * `"weakened"` for lost links and for persistent links whose strength
#'   decreases by more than `strength_tolerance`;
#' * `"unchanged"` for persistent links whose absolute strength change does not
#'   exceed `strength_tolerance`.
#'
#' `Strength_direction` is calculated for both binary and weighted networks
#' and is used together with `Direction` and `Mechanism` to define the
#' categories reported in `structural`. The tolerance affects only the
#' classification of persistent links.
#'
#' Empty categories have zero link counts. For weighted networks, empty
#' categories also have zero aggregated strength, whereas mean strength
#' statistics are reported as `NA`.
#'
#' @export
quantify_network_changes <- function(
    x,
    site_1,
    time_1,
    site_2,
    time_2,
    node_classification = NULL,
    link_classification = NULL,
    active_only = TRUE,
    strength_tolerance = 0
) {
  validate_temporal_interactions(x)
  validate_flag(active_only, "active_only")
  
  if (
    !is.numeric(strength_tolerance) ||
    length(strength_tolerance) != 1L ||
    is.na(strength_tolerance) ||
    !is.finite(strength_tolerance) ||
    strength_tolerance < 0
  ) {
    stop(
      "`strength_tolerance` must be a single finite, non-negative ",
      "numeric value.",
      call. = FALSE
    )
  }
  
  # -----------------------------------------------------------------------
  # Classify nodes
  # -----------------------------------------------------------------------
  
  if (is.null(node_classification)) {
    nodes <- classify_network_nodes(
      x = x,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2,
      active_only = active_only
    )
  } else {
    validate_node_classification(
      node_status = node_classification,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2
    )
    
    nodes <- node_classification
  }
  
  # -----------------------------------------------------------------------
  # Classify links
  # -----------------------------------------------------------------------
  
  if (is.null(link_classification)) {
    links <- classify_network_links(
      x = x,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2,
      node_status = nodes,
      active_only = active_only
    )
  } else {
    validate_link_classification(
      link_classification = link_classification,
      site_1 = site_1,
      time_1 = time_1,
      site_2 = site_2,
      time_2 = time_2
    )
    
    links <- link_classification
  }
  
  if (nrow(links) == 0L) {
    stop(
      if (isTRUE(active_only)) {
        paste0(
          "No active links remain in either network after ",
          "applying `active_only = TRUE`."
        )
      } else {
        "No links are available for the requested network comparison."
      },
      call. = FALSE
    )
  }
  
  link_nodes <- unique(c(
    as.character(links$Origin_node),
    as.character(links$Destiny_node)
  ))
  
  if (!setequal(
    as.character(nodes$Node),
    link_nodes
  )) {
    stop(
      "`node_classification` and `link_classification` ",
      "do not contain the same set of nodes.",
      call. = FALSE
    )
  }
  
  node_lookup <- stats::setNames(
    as.character(nodes$Node_status),
    as.character(nodes$Node)
  )
  
  expected_origin_status <- unname(
    node_lookup[as.character(links$Origin_node)]
  )
  
  expected_destiny_status <- unname(
    node_lookup[as.character(links$Destiny_node)]
  )
  
  if (
    anyNA(expected_origin_status) ||
    anyNA(expected_destiny_status) ||
    !identical(
      as.character(links$Origin_status),
      expected_origin_status
    ) ||
    !identical(
      as.character(links$Destiny_status),
      expected_destiny_status
    )
  ) {
    stop(
      "`node_classification` and `link_classification` ",
      "contain inconsistent node statuses.",
      call. = FALSE
    )
  }
  
  # -----------------------------------------------------------------------
  # Classify direction of strength change
  # -----------------------------------------------------------------------
  
  strength_change <-
    links$Strength_network_2 -
    links$Strength_network_1
  
  links$Strength_direction <- ifelse(
    links$Direction == "gained",
    "strengthened",
    ifelse(
      links$Direction == "lost",
      "weakened",
      ifelse(
        abs(strength_change) <= strength_tolerance,
        "unchanged",
        ifelse(
          strength_change > 0,
          "strengthened",
          "weakened"
        )
      )
    )
  )
  
  group_vars <- c(
    "Direction",
    "Mechanism",
    "Strength_direction"
  )
  
  # -----------------------------------------------------------------------
  # Define all valid link-change categories
  # -----------------------------------------------------------------------
  
  category_order <- data.frame(
    Direction = c(
      "persistent",
      "lost",
      "lost",
      "lost",
      "gained",
      "gained",
      "gained"
    ),
    Mechanism = c(
      "permanent",
      "rewiring",
      "one_non_permanent_node",
      "two_non_permanent_nodes",
      "rewiring",
      "one_non_permanent_node",
      "two_non_permanent_nodes"
    ),
    Strength_direction = c(
      "unchanged",
      "weakened",
      "weakened",
      "weakened",
      "strengthened",
      "strengthened",
      "strengthened"
    ),
    stringsAsFactors = FALSE
  )
  
  if (isTRUE(x$network_type$weighted)) {
    
    category_order <- rbind(
      category_order[1L, , drop = FALSE],
      data.frame(
        Direction = c("persistent", "persistent"),
        Mechanism = c("permanent", "permanent"),
        Strength_direction = c("weakened", "strengthened"),
        stringsAsFactors = FALSE
      ),
      category_order[-1L, , drop = FALSE]
    )
  }
  
  category_order$.category_order <-
    seq_len(nrow(category_order))
  
  # -----------------------------------------------------------------------
  # Count links
  # -----------------------------------------------------------------------
  
  observed_counts <- stats::aggregate(
    rep(1L, nrow(links)),
    by = links[group_vars],
    FUN = sum
  )
  
  names(observed_counts)[ncol(observed_counts)] <- "n_links"
  
  structural <- merge(
    category_order,
    observed_counts,
    by = group_vars,
    all.x = TRUE,
    sort = FALSE
  )
  
  structural$n_links[is.na(structural$n_links)] <- 0L
  
  structural <- structural[
    order(structural$.category_order),
    ,
    drop = FALSE
  ]
  
  structural$.category_order <- NULL
  rownames(structural) <- NULL
  
  
  # -----------------------------------------------------------------------
  # Weighted metrics
  # -----------------------------------------------------------------------
  
  if (isTRUE(x$network_type$weighted)) {
    links$Strength_change <- strength_change
    
    links$Absolute_strength_change <-
      abs(links$Strength_change)
    
    # Aggregated strength within each link-change category
    strength_totals <- stats::aggregate(
      cbind(
        Strength_network_1,
        Strength_network_2,
        Strength_change,
        Absolute_strength_change
      ) ~ Direction + Mechanism + Strength_direction,
      data = links,
      FUN = sum
    )
    
    names(strength_totals) <- c(
      "Direction",
      "Mechanism",
      "Strength_direction",
      "total_strength_network_1",
      "total_strength_network_2",
      "net_strength_change",
      "total_absolute_strength_change"
    )
    
    # Mean change per link within each link-change category
    strength_means <- stats::aggregate(
      cbind(
        Strength_change,
        Absolute_strength_change
      ) ~ Direction + Mechanism + Strength_direction,
      data = links,
      FUN = mean
    )
    
    names(strength_means) <- c(
      "Direction",
      "Mechanism",
      "Strength_direction",
      "mean_strength_change",
      "mean_absolute_strength_change"
    )
    
    structural$.category_order <- seq_len(nrow(structural))
    
    structural <- merge(
      structural,
      strength_totals,
      by = group_vars,
      all.x = TRUE,
      sort = FALSE
    )
    
    structural <- merge(
      structural,
      strength_means,
      by = group_vars,
      all.x = TRUE,
      sort = FALSE
    )
    
    structural <- structural[
      order(structural$.category_order),
      ,
      drop = FALSE
    ]
    
    structural$.category_order <- NULL
    rownames(structural) <- NULL
    
    # Empty link-change categories have zero aggregate strength.
    total_columns <- c(
      "total_strength_network_1",
      "total_strength_network_2",
      "net_strength_change",
      "total_absolute_strength_change"
    )
    
    for (column in total_columns) {
      structural[[column]][is.na(structural[[column]])] <- 0
    }
    
    # Means remain NA when the category contains no links.
  }
  
  # -----------------------------------------------------------------------
  # Comparison metadata
  # -----------------------------------------------------------------------
  
  comparison_type <- unique(
    as.character(links$Comparison_type)
  )
  
  if (length(comparison_type) != 1L) {
    stop(
      "The link classification contains inconsistent comparison types.",
      call. = FALSE
    )
  }
  
  comparison <- list(
    site_1 = site_1,
    time_1 = time_1,
    site_2 = site_2,
    time_2 = time_2,
    comparison_type = comparison_type,
    directed = x$network_type$directed,
    topology = x$network_type$topology,
    weighted = x$network_type$weighted,
    n_nodes_network_1 = sum(nodes$Present_network_1),
    n_nodes_network_2 = sum(nodes$Present_network_2),
    n_nodes_union = nrow(nodes),
    n_links_network_1 = sum(links$Present_network_1),
    n_links_network_2 = sum(links$Present_network_2),
    n_links_union = nrow(links)
  )
  
  return(new_network_changes(
    comparison = comparison,
    structural = structural,
    links = links,
    nodes = nodes
  ))
}