# =============================================================================
# Validation of node classification tables
# =============================================================================

#' Validate a node classification table
#'
#' Checks that a node classification table has the structure and contents
#' required by `classify_network_links()`.
#'
#' The function verifies that `node_status` is a data frame containing the
#' required network identifiers, one row per node, and valid node
#' classifications. It also checks that the table corresponds to the same
#' ordered network comparison requested by the calling function.
#'
#' @param node_status A data frame returned by
#'   [classify_network_nodes()].
#' @param site_1 A single value identifying the site of network 1.
#' @param time_1 A single value identifying the time of network 1.
#' @param site_2 A single value identifying the site of network 2.
#' @param time_2 A single value identifying the time of network 2.
#'
#' @return `node_status`, invisibly, when the classification table is valid.
#'   An error is raised otherwise.
#'
#' @details
#' The function requires the following columns:
#'
#' * `Site_1`
#' * `Time_1`
#' * `Site_2`
#' * `Time_2`
#' * `Comparison_type`
#' * `Node`
#' * `Guild`
#' * `Present_network_1`
#' * `Present_network_2`
#' * `Node_status`
#'
#' Valid values of `Node_status` are:
#'
#' * `"permanent"`: the node is present in both networks;
#' * `"lost"`: the node is present only in network 1;
#' * `"gained"`: the node is present only in network 2.
#'
#' The comparison is directional. Therefore, all rows in `node_status` must
#' correspond to the ordered transition defined by `site_1`, `time_1`,
#' `site_2`, and `time_2`.
#'
#' `Comparison_type` must be one of `"temporal"`, `"spatial"`, or
#' `"spatiotemporal"` and must be consistent with the requested comparison:
#'
#' * `"temporal"` when both networks belong to the same site but different
#'   times;
#' * `"spatial"` when both networks belong to different sites at the same time;
#' * `"spatiotemporal"` when both site and time differ.
#' 
#' The function also verifies that each node occurs only once and that
#' `Node_status` is consistent with `Present_network_1` and
#' `Present_network_2`. It does not check the classifications against the
#' original interaction data.
#'
#' @keywords internal
validate_node_classification <- function(
    node_status,
    site_1,
    time_1,
    site_2,
    time_2
) {
  if (!is.data.frame(node_status)) {
    stop(
      "`node_status` must be a data frame.",
      call. = FALSE
    )
  }
  
  if (nrow(node_status) == 0L) {
    stop(
      "`node_status` must contain at least one node.",
      call. = FALSE
    )
  }
  
  required <- c(
    "Site_1",
    "Time_1",
    "Site_2",
    "Time_2",
    "Comparison_type",
    "Node",
    "Guild",
    "Present_network_1",
    "Present_network_2",
    "Node_status"
  )
  
  missing <- setdiff(required, names(node_status))
  
  if (length(missing) > 0L) {
    stop(
      "`node_status` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  presence_columns <- c(
    "Present_network_1",
    "Present_network_2"
  )
  
  for (column in presence_columns) {
    if (
      !is.logical(node_status[[column]]) ||
      anyNA(node_status[[column]])
    ) {
      stop(
        "`", column,
        "` must contain non-missing logical values.",
        call. = FALSE
      )
    }
  }
  
  if (any(
    !node_status$Present_network_1 &
    !node_status$Present_network_2
  )) {
    stop(
      "Every classified node must occur in at least one network.",
      call. = FALSE
    )
  }
  
  
  invalid_nodes <-
    is.na(node_status$Node) |
    trimws(as.character(node_status$Node)) == ""
  
  if (any(invalid_nodes)) {
    stop(
      "`node_status$Node` contains missing or empty identifiers.",
      call. = FALSE
    )
  }
  
  valid_status <- c(
    "permanent",
    "lost",
    "gained"
  )
  
  if (
    anyNA(node_status$Node_status) ||
    !all(node_status$Node_status %in% valid_status)
  ) {
    stop(
      "`node_status$Node_status` contains invalid classifications.",
      call. = FALSE
    )
  }
  
  expected_status <- ifelse(
    node_status$Present_network_1 &
      node_status$Present_network_2,
    "permanent",
    ifelse(
      node_status$Present_network_1,
      "lost",
      "gained"
    )
  )
  
  if (
    !identical(
      as.character(node_status$Node_status),
      expected_status
    )
  ) {
    stop(
      "`Node_status` is inconsistent with the node-presence indicators.",
      call. = FALSE
    )
  }
  
  same_comparison <-
    all(node_status$Site_1 == site_1) &&
    all(node_status$Time_1 == time_1) &&
    all(node_status$Site_2 == site_2) &&
    all(node_status$Time_2 == time_2)
  
  if (!isTRUE(same_comparison)) {
    stop(
      "`node_status` does not correspond to the requested network comparison.",
      call. = FALSE
    )
  }
  
  same_site <- isTRUE(site_1 == site_2)
  same_time <- isTRUE(time_1 == time_2)
  
  expected_comparison_type <- if (
    same_site && !same_time
  ) {
    "temporal"
  } else if (
    !same_site && same_time
  ) {
    "spatial"
  } else {
    "spatiotemporal"
  }
  
  valid_comparison_types <- c(
    "temporal",
    "spatial",
    "spatiotemporal"
  )
  
  if (
    anyNA(node_status$Comparison_type) ||
    !all(
      node_status$Comparison_type %in%
      valid_comparison_types
    )
  ) {
    stop(
      "`Comparison_type` contains invalid values.",
      call. = FALSE
    )
  }
  
  if (
    !isTRUE(all(
      node_status$Comparison_type ==
      expected_comparison_type
    ))
  ) {
    stop(
      "`Comparison_type` is inconsistent with the requested comparison.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(node_status$Node)) {
    stop(
      "`node_status` must contain one row per node.",
      call. = FALSE
    )
  }
  
  invisible(node_status)
}