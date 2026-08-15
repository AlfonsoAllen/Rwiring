# =============================================================================
# Definition and methods of the network_changes class
# =============================================================================


# -------------------------------------------------------------------------
# Constructor
# -------------------------------------------------------------------------

#' Construct a network changes object
#'
#' Low-level constructor for objects of class `network_changes`.
#'
#' The constructor stores the comparison metadata, link-change summary,
#' link-level classification, and node-level classification generated when
#' comparing two interaction networks.
#'
#' This function is intended for internal use by
#' [quantify_network_changes()]. Users should normally create
#' `network_changes` objects through that function.
#'
#' @param comparison A named list containing metadata describing the network
#'   comparison.
#' @param structural A data frame containing link counts for each valid
#'   link-change category and, for weighted networks, aggregated and mean
#'   strength-change summaries.
#' @param links A data frame containing one row per link occurring in at least
#'   one of the compared networks.
#' @param nodes A data frame containing one row per node occurring in at least
#'   one of the compared networks.
#'
#' @return An object of class `network_changes`.
#'
#' @keywords internal
new_network_changes <- function(
    comparison,
    structural,
    links,
    nodes
) {
  x <- structure(
    list(
      comparison = comparison,
      structural = structural,
      links = links,
      nodes = nodes
    ),
    class = "network_changes"
  )
  
  validate_network_changes(x)
  
  x
}


# -------------------------------------------------------------------------
# Validation
# -------------------------------------------------------------------------

#' Validate a network changes object
#'
#' Checks that an object has the structure and contents required by the
#' `network_changes` class.
#'
#' The function verifies that:
#'
#' * the object inherits from `network_changes`;
#' * the object contains `comparison`, `structural`, `links`, and `nodes`;
#' * the three tabular components are data frames;
#' * the comparison metadata contain the required fields;
#' * the `structural` table contains the required link-change classification
#'   columns and link counts;
#' * the link and node tables contain the required identifiers and
#'   classifications; and
#' * the principal node and link counts are consistent across the object.
#'
#' @param x An object expected to inherit from `network_changes`.
#'
#' @return `x`, invisibly, when the object is valid. An error is raised
#'   otherwise.
#'
#' @keywords internal
validate_network_changes <- function(x) {
  if (!inherits(x, "network_changes")) {
    stop(
      "`x` must inherit from class `network_changes`.",
      call. = FALSE
    )
  }
  
  if (!is.list(x)) {
    stop(
      "Invalid `network_changes` object: the object must be a list.",
      call. = FALSE
    )
  }
  
  required_components <- c(
    "comparison",
    "structural",
    "links",
    "nodes"
  )
  
  missing_components <- setdiff(
    required_components,
    names(x)
  )
  
  if (length(missing_components) > 0L) {
    stop(
      "Invalid `network_changes` object. Missing components: ",
      paste(missing_components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.list(x$comparison)) {
    stop(
      "`comparison` must be a list.",
      call. = FALSE
    )
  }
  
  table_components <- c(
    "structural",
    "links",
    "nodes"
  )
  
  for (component in table_components) {
    if (!is.data.frame(x[[component]])) {
      stop(
        "`", component, "` must be a data frame.",
        call. = FALSE
      )
    }
  }
  
  if (nrow(x$structural) == 0L) {
    stop(
      "`structural` must contain at least one link-change category.",
      call. = FALSE
    )
  }
  
  if (nrow(x$links) == 0L) {
    stop(
      "`links` must contain at least one link.",
      call. = FALSE
    )
  }
  
  if (nrow(x$nodes) == 0L) {
    stop(
      "`nodes` must contain at least one node.",
      call. = FALSE
    )
  }
  
  required_comparison_fields <- c(
    "site_1",
    "time_1",
    "site_2",
    "time_2",
    "comparison_type",
    "directed",
    "topology",
    "weighted",
    "n_nodes_network_1",
    "n_nodes_network_2",
    "n_nodes_union",
    "n_links_network_1",
    "n_links_network_2",
    "n_links_union"
  )
  
  missing_comparison_fields <- setdiff(
    required_comparison_fields,
    names(x$comparison)
  )
  
  if (length(missing_comparison_fields) > 0L) {
    stop(
      "Invalid `comparison` metadata. Missing fields: ",
      paste(missing_comparison_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  valid_comparison_types <- c(
    "temporal",
    "spatial",
    "spatiotemporal"
  )
  
  if (
    length(x$comparison$comparison_type) != 1L ||
    is.na(x$comparison$comparison_type) ||
    !x$comparison$comparison_type %in%
    valid_comparison_types
  ) {
    stop(
      "`comparison_type` must be `\"temporal\"`, `\"spatial\"`, ",
      "or `\"spatiotemporal\"`.",
      call. = FALSE
    )
  }
  
  logical_metadata <- c(
    "directed",
    "weighted"
  )
  
  for (field in logical_metadata) {
    value <- x$comparison[[field]]
    
    if (
      !is.logical(value) ||
      length(value) != 1L ||
      is.na(value)
    ) {
      stop(
        "`comparison$", field,
        "` must be a single non-missing logical value.",
        call. = FALSE
      )
    }
  }
  
  if (
    length(x$comparison$topology) != 1L ||
    is.na(x$comparison$topology) ||
    !x$comparison$topology %in%
    c("unipartite", "multipartite")
  ) {
    stop(
      "`comparison$topology` must be `\"unipartite\"` or ",
      "`\"multipartite\"`.",
      call. = FALSE
    )
  }
  
  count_fields <- c(
    "n_nodes_network_1",
    "n_nodes_network_2",
    "n_nodes_union",
    "n_links_network_1",
    "n_links_network_2",
    "n_links_union"
  )
  
  for (field in count_fields) {
    value <- x$comparison[[field]]
    
    if (
      !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value != floor(value)
    ) {
      stop(
        "`comparison$", field,
        "` must be a single non-negative whole number.",
        call. = FALSE
      )
    }
  }
  
  required_structural_columns <- c(
    "Direction",
    "Mechanism",
    "Strength_direction",
    "n_links"
  )
  
  missing_structural_columns <- setdiff(
    required_structural_columns,
    names(x$structural)
  )
  
  if (length(missing_structural_columns) > 0L) {
    stop(
      "`structural` is missing required columns: ",
      paste(missing_structural_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  required_link_columns <- c(
    "Origin_node",
    "Destiny_node",
    "Present_network_1",
    "Present_network_2",
    "Strength_network_1",
    "Strength_network_2",
    "Direction",
    "Mechanism",
    "Strength_direction"
  )
  
  missing_link_columns <- setdiff(
    required_link_columns,
    names(x$links)
  )
  
  if (length(missing_link_columns) > 0L) {
    stop(
      "`links` is missing required columns: ",
      paste(missing_link_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  required_node_columns <- c(
    "Node",
    "Present_network_1",
    "Present_network_2",
    "Node_status"
  )
  
  missing_node_columns <- setdiff(
    required_node_columns,
    names(x$nodes)
  )
  
  if (length(missing_node_columns) > 0L) {
    stop(
      "`nodes` is missing required columns: ",
      paste(missing_node_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (
    sum(x$structural$n_links) !=
    nrow(x$links)
  ) {
    stop(
      "The structural link counts do not equal the number of rows in ",
      "`links`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_nodes_union !=
    nrow(x$nodes)
  ) {
    stop(
      "`comparison$n_nodes_union` does not equal the number of rows in ",
      "`nodes`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_links_union !=
    nrow(x$links)
  ) {
    stop(
      "`comparison$n_links_union` does not equal the number of rows in ",
      "`links`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_nodes_network_1 !=
    sum(x$nodes$Present_network_1)
  ) {
    stop(
      "`comparison$n_nodes_network_1` is inconsistent with `nodes`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_nodes_network_2 !=
    sum(x$nodes$Present_network_2)
  ) {
    stop(
      "`comparison$n_nodes_network_2` is inconsistent with `nodes`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_links_network_1 !=
    sum(x$links$Present_network_1)
  ) {
    stop(
      "`comparison$n_links_network_1` is inconsistent with `links`.",
      call. = FALSE
    )
  }
  
  if (
    x$comparison$n_links_network_2 !=
    sum(x$links$Present_network_2)
  ) {
    stop(
      "`comparison$n_links_network_2` is inconsistent with `links`.",
      call. = FALSE
    )
  }
  
  
  invisible(x)
}


# -------------------------------------------------------------------------
# S3 methods
# -------------------------------------------------------------------------

#' Print a network changes object
#'
#' Displays the main characteristics of a comparison between two interaction
#' networks, including the comparison type, network properties, and the
#' numbers of nodes and links in each network and in their union.
#'
#' @param x A `network_changes` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [quantify_network_changes()]
#'
#' @method print network_changes
#' @rdname network_changes
#' @export
print.network_changes <- function(
    x,
    ...
) {
  validate_network_changes(x)
  
  comparison <- x$comparison
  
  cat("<network_changes>\n")
  cat(
    "  Comparison: ",
    comparison$comparison_type,
    "\n",
    sep = ""
  )
  cat(
    "  Network 1:  ",
    comparison$site_1,
    " / ",
    comparison$time_1,
    "\n",
    sep = ""
  )
  cat(
    "  Network 2:  ",
    comparison$site_2,
    " / ",
    comparison$time_2,
    "\n",
    sep = ""
  )
  cat(
    "  Directed:   ",
    comparison$directed,
    "\n",
    sep = ""
  )
  cat(
    "  Topology:   ",
    comparison$topology,
    "\n",
    sep = ""
  )
  cat(
    "  Weighted:   ",
    comparison$weighted,
    "\n",
    sep = ""
  )
  
  cat("\nNodes\n")
  cat(
    "  Network 1:  ",
    comparison$n_nodes_network_1,
    "\n",
    sep = ""
  )
  cat(
    "  Network 2:  ",
    comparison$n_nodes_network_2,
    "\n",
    sep = ""
  )
  cat(
    "  Union:      ",
    comparison$n_nodes_union,
    "\n",
    sep = ""
  )
  
  cat("\nLinks\n")
  cat(
    "  Network 1:  ",
    comparison$n_links_network_1,
    "\n",
    sep = ""
  )
  cat(
    "  Network 2:  ",
    comparison$n_links_network_2,
    "\n",
    sep = ""
  )
  cat(
    "  Union:      ",
    comparison$n_links_union,
    "\n",
    sep = ""
  )
  
  cat("\nUse `summary()` to inspect link changes.\n")
  
  invisible(x)
}


#' Summarize a network changes object
#'
#' Creates a compact summary of link changes between two interaction networks.
#'
#' The returned object contains the comparison metadata and the link-change
#' summary. For weighted networks, the structural table also contains the
#' aggregated and mean strength-change metrics calculated by
#' [quantify_network_changes()].
#'
#' @param object A `network_changes` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return An object of class `summary.network_changes`, represented as a
#'   named list containing:
#'
#' * `comparison`: metadata describing the network comparison;
#' * `structural`: link counts for each valid link-change category and,
#'   when applicable, aggregated and mean strength-change metrics.
#'
#' @method summary network_changes
#' @rdname network_changes
#' @export
summary.network_changes <- function(
    object,
    ...
) {
  validate_network_changes(object)
  
  result <- list(
    comparison = object$comparison,
    structural = object$structural
  )
  
  class(result) <- "summary.network_changes"
  
  result
}


#' Print a network changes summary
#'
#' Prints the comparison metadata and structural summary returned by
#' [summary.network_changes()].
#'
#' @param x An object of class `summary.network_changes`.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @method print summary.network_changes
#' @rdname network_changes
#' @export
print.summary.network_changes <- function(
    x,
    ...
) {
  comparison <- x$comparison
  structural <- x$structural
  
  cat("Summary of network changes\n")
  cat("==========================\n\n")
  
  cat(
    "Comparison type: ",
    comparison$comparison_type,
    "\n",
    sep = ""
  )
  cat(
    "Network 1:       ",
    comparison$site_1,
    " / ",
    comparison$time_1,
    "\n",
    sep = ""
  )
  cat(
    "Network 2:       ",
    comparison$site_2,
    " / ",
    comparison$time_2,
    "\n",
    sep = ""
  )
  cat(
    "Directed:        ",
    comparison$directed,
    "\n",
    sep = ""
  )
  cat(
    "Topology:        ",
    comparison$topology,
    "\n",
    sep = ""
  )
  cat(
    "Weighted:        ",
    comparison$weighted,
    "\n",
    sep = ""
  )
  
  cat("\nLink changes\n")
  cat("------------\n")
  
  print(
    structural,
    row.names = FALSE
  )
  
  invisible(x)
}