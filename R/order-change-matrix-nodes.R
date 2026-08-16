# =============================================================================
# Ordering of nodes for network-change matrices
# =============================================================================

#' Order nodes for network-change matrices
#'
#' Orders nodes for matrix-based representations of network changes.
#'
#' Nodes are grouped according to `Node_status` in the order
#' `"permanent"`, `"lost"`, and `"gained"`. Within each status group,
#' nodes are ordered alphabetically by their identifier.
#'
#' This function is used internally by [build_network_change_matrix()] to
#' obtain a consistent node ordering for interaction and adjacency matrices.
#'
#' @param nodes A data frame containing node classifications, as stored in
#'   the `nodes` component of a `network_changes` object. The data frame must
#'   contain the columns `Node` and `Node_status`.
#'
#' @return The input data frame with rows reordered first by `Node_status`
#'   (`"permanent"`, `"lost"`, `"gained"`) and then alphabetically by `Node`
#'   within each status group. All original columns are preserved.
#'
#' @keywords internal
order_change_matrix_nodes <- function(nodes) {
  
  required_columns <- c(
    "Node",
    "Node_status"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(nodes)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "`nodes` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  status_levels <- c(
    "permanent",
    "lost",
    "gained"
  )
  
  invalid_status <- setdiff(
    unique(as.character(nodes$Node_status)),
    status_levels
  )
  
  if (length(invalid_status) > 0L) {
    stop(
      "`Node_status` contains invalid values: ",
      paste(invalid_status, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  status_order <- match(
    as.character(nodes$Node_status),
    status_levels
  )
  
  ordered_rows <- order(
    status_order,
    as.character(nodes$Node)
  )
  
  nodes <- nodes[
    ordered_rows,
    ,
    drop = FALSE
  ]
  
  rownames(nodes) <- NULL
  
  nodes
}