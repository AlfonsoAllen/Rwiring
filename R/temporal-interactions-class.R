# =============================================================================
# Definition and methods of the temporal_interactions class
# =============================================================================


# -------------------------------------------------------------------------
# Constructor
# -------------------------------------------------------------------------
#' Construct a temporal interactions object
#'
#' Low-level constructor for objects of class
#' [`temporal_interactions`][prepare_interactions]. It stores a standardized
#' interaction table together with metadata describing the network and
#' diagnostics generated during data preparation.
#'
#' This constructor performs basic consistency checks but does not carry out
#' the full validation and standardization applied by
#' [prepare_interactions()]. Users should normally create objects through
#' [prepare_interactions()] rather than calling this function directly.
#'
#' @param interactions A data frame containing the standardized interaction
#'   table. It must contain at least the columns `Site`, `Time`,
#'   `Origin_node`, `Destiny_node`, and `Strength`.
#' @param directed A single non-missing logical value indicating whether the
#'   network is directed.
#' @param topology A single character value, either `"unipartite"` or
#'   `"multipartite"`.
#' @param weighted A single non-missing logical value indicating whether
#'   `Strength` represents quantitative interaction weights rather than binary
#'   presence.
#' @param allow_self_links A single non-missing logical value indicating
#'   whether self-links are allowed. This can be `TRUE` only for unipartite
#'   networks.
#' @param has_guilds A single non-missing logical value indicating whether
#'   guild information was supplied.
#' @param n_guilds A single integer giving the number of guilds. It must be
#'   `NA_integer_` when `has_guilds = FALSE`.
#' @param guilds A character vector containing the guild names, or `NULL` when
#'   guild information was not supplied.
#' @param diagnostics A named list containing information about transformations
#'   carried out during data preparation.
#'
#' @return An object of class `temporal_interactions`.
#'
#' @keywords internal
new_temporal_interactions <- function(
    interactions,
    directed,
    topology,
    weighted,
    allow_self_links,
    has_guilds,
    n_guilds,
    guilds = NULL,
    diagnostics = list()
) {
  stopifnot(
    is.data.frame(interactions),
    is.logical(directed),
    length(directed) == 1L,
    !is.na(directed),
    is.character(topology),
    length(topology) == 1L,
    !is.na(topology),
    topology %in% c("unipartite", "multipartite"),
    is.logical(weighted),
    length(weighted) == 1L,
    !is.na(weighted),
    is.logical(allow_self_links),
    length(allow_self_links) == 1L,
    !is.na(allow_self_links),
    is.logical(has_guilds),
    length(has_guilds) == 1L,
    !is.na(has_guilds),
    is.integer(n_guilds),
    length(n_guilds) == 1L,
    is.null(guilds) || is.character(guilds),
    is.list(diagnostics)
  )
  
  if (topology == "multipartite" && allow_self_links) {
    stop(
      "`allow_self_links` must be FALSE for multipartite networks.",
      call. = FALSE
    )
  }
  
  if (topology == "multipartite" && !has_guilds) {
    stop(
      "A multipartite network must contain guild information.",
      call. = FALSE
    )
  }
  
  if (has_guilds && is.null(guilds)) {
    stop(
      "`guilds` cannot be NULL when `has_guilds = TRUE`.",
      call. = FALSE
    )
  }
  
  if (has_guilds && length(guilds) == 0L) {
    stop(
      "`guilds` must contain at least one guild when ",
      "`has_guilds = TRUE`.",
      call. = FALSE
    )
  }
  
  if (!has_guilds && !is.null(guilds)) {
    stop(
      "`guilds` must be NULL when `has_guilds = FALSE`.",
      call. = FALSE
    )
  }
  
  if (!has_guilds && !is.na(n_guilds)) {
    stop(
      "`n_guilds` must be `NA_integer_` when `has_guilds = FALSE`.",
      call. = FALSE
    )
  }
  
  if (has_guilds && is.na(n_guilds)) {
    stop(
      "`n_guilds` cannot be missing when `has_guilds = TRUE`.",
      call. = FALSE
    )
  }
  
  if (has_guilds && n_guilds != length(guilds)) {
    stop(
      "`n_guilds` must equal the number of values in `guilds`.",
      call. = FALSE
    )
  }
  
  structure(
    list(
      interactions = interactions,
      network_type = list(
        directed = directed,
        topology = topology,
        weighted = weighted,
        allow_self_links = allow_self_links,
        has_guilds = has_guilds,
        n_guilds = n_guilds,
        guilds = guilds
      ),
      diagnostics = diagnostics
    ),
    class = "temporal_interactions"
  )
}

# -------------------------------------------------------------------------
# Validation
# -------------------------------------------------------------------------
#' Validate a temporal interactions object
#'
#' Checks that an object has the structure and metadata required by the
#' `temporal_interactions` class.
#'
#' The function verifies that:
#'
#' * the object inherits from `temporal_interactions`;
#' * the standardized interaction table is present;
#' * all mandatory interaction columns exist;
#' * the network metadata contain the required fields;
#' * self-link permissions are valid and consistent with network topology; and
#' * guild columns are present whenever the object declares guild information.
#'
#' This function is intended for internal use by class methods.
#'
#' @param x An object expected to inherit from `temporal_interactions`.
#'
#' @return `x`, invisibly, when the object is valid. An error is raised
#'   otherwise.
#'
#' @keywords internal
validate_temporal_interactions <- function(x) {
  
  if (!inherits(x, "temporal_interactions")) {
    stop(
      "`x` must inherit from class `temporal_interactions`.",
      call. = FALSE
    )
  }
  
  if (!is.list(x) || !is.data.frame(x$interactions)) {
    stop(
      "Invalid `temporal_interactions` object structure.",
      call. = FALSE
    )
  }
  
  
  required_network_fields <- c(
    "directed",
    "topology",
    "weighted",
    "allow_self_links",
    "has_guilds",
    "n_guilds",
    "guilds"
  )
  
  if (
    !is.list(x$network_type) ||
    !all(required_network_fields %in% names(x$network_type))
  ) {
    stop(
      "Invalid `temporal_interactions` object: malformed ",
      "`network_type` metadata.",
      call. = FALSE
    )
  }
  
  required_columns <- c(
    "Site",
    "Time",
    "Origin_node",
    "Destiny_node",
    "Strength"
  )
  
  missing <- setdiff(
    required_columns,
    names(x$interactions)
  )
  
  if (length(missing) > 0L) {
    stop(
      "Invalid `temporal_interactions` object. Missing columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (
    isTRUE(x$network_type$has_guilds) &&
    !all(c("Origin_guild", "Destiny_guild") %in%
         names(x$interactions))
  ) {
    stop(
      "Invalid `temporal_interactions` object: guild metadata ",
      "are present but guild columns are missing.",
      call. = FALSE
    )
  }
  
  if (
    !is.logical(x$network_type$allow_self_links) ||
    length(x$network_type$allow_self_links) != 1L ||
    is.na(x$network_type$allow_self_links)
  ) {
    stop(
      "`network_type$allow_self_links` must be a single non-missing ",
      "logical value.",
      call. = FALSE
    )
  }
  
  if (
    x$network_type$topology == "multipartite" &&
    isTRUE(x$network_type$allow_self_links)
  ) {
    stop(
      "Multipartite networks cannot allow self-links.",
      call. = FALSE
    )
  }
  
  if (
    !is.character(x$network_type$topology) ||
    length(x$network_type$topology) != 1L ||
    is.na(x$network_type$topology) ||
    !x$network_type$topology %in% c("unipartite", "multipartite")
  ) {
    stop(
      "`network_type$topology` must be `\"unipartite\"` or `\"multipartite\"`.",
      call. = FALSE
    )
  }
  
  invisible(x)
}

# -------------------------------------------------------------------------
# S3 methods
# -------------------------------------------------------------------------
#' Print a temporal interactions object
#'
#' Displays the principal characteristics of a
#' `temporal_interactions` object, including the number of rows, sites and time
#' points, network type, self-link permissions, detected guilds, and all node
#' identifiers.
#'
#' Guild and node names are displayed alphabetically and enclosed in quotation
#' marks. This allows users to inspect the exact identifiers retained by
#' [prepare_interactions()] and to detect possible inconsistencies such as
#' trailing spaces, differences in capitalization, or typographical errors.
#'
#' @param x A `temporal_interactions` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [prepare_interactions()], [summary.temporal_interactions()],
#'   [as.data.frame.temporal_interactions()]
#'
#' @examples
#' interactions <- data.frame(
#'   Site = c("site_1", "site_1"),
#'   Time = c(2020, 2021),
#'   Origin_node = c("plant_1", "plant_1"),
#'   Destiny_node = c("bee_1", "bee_2"),
#'   Origin_guild = c("plant", "plant"),
#'   Destiny_guild = c("pollinator", "pollinator"),
#'   Strength = c(4, 7)
#' )
#'
#' prepared <- prepare_interactions(
#'   interactions,
#'   directed = FALSE,
#'   multipartite = TRUE
#' )
#'
#' print(prepared)
#'
#' @method print temporal_interactions
#' @rdname temporal_interactions
#' @export
print.temporal_interactions <- function(
    x,
    ...
) {
  validate_temporal_interactions(x)
  
  data <- x$interactions
  type <- x$network_type
  
  cat("<temporal_interactions>\n")
  cat("  Rows:       ", nrow(data), "\n", sep = "")
  cat("  Sites:      ", length(unique(data$Site)), "\n", sep = "")
  cat("  Times:      ", length(unique(data$Time)), "\n", sep = "")
  cat("  Directed:   ", type$directed, "\n", sep = "")
  cat("  Topology:   ", type$topology, "\n", sep = "")
  cat("  Weighted:   ", type$weighted, "\n", sep = "")
  cat(
    "  Self-links: ",
    if (type$allow_self_links) "allowed" else "not allowed",
    "\n",
    sep = ""
  )
  cat("  Has guilds: ", type$has_guilds, "\n", sep = "")
  
  
  # Print guild names
  if (isTRUE(type$has_guilds)) {
    guild_names <- sort(type$guilds)
    
    cat("\nGuild names (", length(guild_names), "):\n", sep = "")
    cat("  ",
        paste(sprintf('"%s"', guild_names), collapse = ", "),
        "\n",
        sep = "")
  }
  
  # Print node names
  node_names <- sort(unique(c(
    as.character(data$Origin_node),
    as.character(data$Destiny_node)
  )))
  
  cat("\nNode names (", length(node_names), "):\n", sep = "")
  cat("  ",
      paste(sprintf('"%s"', node_names), collapse = ", "),
      "\n",
      sep = "")
  
  invisible(x)
}

#' Summarize a temporal interactions object
#'
#' Computes a compact summary of a `temporal_interactions` object.
#'
#' The summary includes:
#'
#' * the number of interaction records;
#' * the number of sites and time points;
#' * the number of site-time networks;
#' * the number of unique nodes;
#' * the number of recorded and unique links;
#' * total, mean and median interaction strength;
#' * network type, self-link permissions, and guild information; and
#' * diagnostics generated during data preparation.
#'
#' A recorded link corresponds to one standardized row associated with a
#' particular site and time point. A unique link is defined by the pair
#' `Origin_node`–`Destiny_node`, independently of site and time.
#'
#' @param object A `temporal_interactions` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return An object of class `summary.temporal_interactions`, represented as a
#'   named list containing network summary statistics and diagnostics.
#'
#' @examples
#' interactions <- data.frame(
#'   Site = c("site_1", "site_1", "site_2"),
#'   Time = c(2020, 2021, 2020),
#'   Origin_node = c("plant_1", "plant_1", "plant_2"),
#'   Destiny_node = c("bee_1", "bee_2", "bee_1"),
#'   Strength = c(4, 7, 2)
#' )
#'
#' prepared <- prepare_interactions(
#'   interactions,
#'   directed = FALSE
#' )
#'
#' summary(prepared)
#'
#' @method summary temporal_interactions
#' @rdname temporal_interactions
#' @export
summary.temporal_interactions <- function(
    object,
    ...
) {
  validate_temporal_interactions(object)
  
  data <- object$interactions
  type <- object$network_type
  
  networks <- unique(data[c("Site", "Time")])
  
  nodes <- unique(c(
    as.character(data$Origin_node),
    as.character(data$Destiny_node)
  ))
  
  link_keys <- c("Origin_node", "Destiny_node")
  unique_links <- unique(data[link_keys])
  
  result <- list(
    n_rows = nrow(data),
    n_sites = length(unique(data$Site)),
    n_times = length(unique(data$Time)),
    n_networks = nrow(networks),
    n_nodes = length(nodes),
    n_link_records = nrow(data),
    n_unique_links = nrow(unique_links),
    total_strength = sum(data$Strength),
    mean_strength = mean(data$Strength),
    median_strength = stats::median(data$Strength),
    directed = type$directed,
    topology = type$topology,
    weighted = type$weighted,
    allow_self_links = type$allow_self_links,
    has_guilds = type$has_guilds,
    n_guilds = type$n_guilds,
    guilds = type$guilds,
    diagnostics = object$diagnostics
  )
  
  class(result) <- "summary.temporal_interactions"
  result
}

#' Print a temporal interactions summary
#'
#' Prints the summary statistics and preparation diagnostics returned by
#' [summary.temporal_interactions()].
#'
#' @param x An object of class `summary.temporal_interactions`.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @method print summary.temporal_interactions
#' @rdname temporal_interactions
#' @export
print.summary.temporal_interactions <- function(
    x,
    ...
) {
  cat("Summary of temporal interactions\n")
  cat("-------------------------------\n")
  cat("Rows:            ", x$n_rows, "\n", sep = "")
  cat("Sites:           ", x$n_sites, "\n", sep = "")
  cat("Time points:     ", x$n_times, "\n", sep = "")
  cat("Networks:        ", x$n_networks, "\n", sep = "")
  cat("Nodes:           ", x$n_nodes, "\n", sep = "")
  cat("Recorded links:  ", x$n_link_records, "\n", sep = "")
  cat("Unique links:    ", x$n_unique_links, "\n", sep = "")
  cat("Total strength:  ", x$total_strength, "\n", sep = "")
  cat("Mean strength:   ", signif(x$mean_strength, 4), "\n", sep = "")
  cat("Median strength: ", signif(x$median_strength, 4), "\n", sep = "")
  cat("Directed:        ", x$directed, "\n", sep = "")
  cat("Topology:        ", x$topology, "\n", sep = "")
  cat("Weighted:        ", x$weighted, "\n", sep = "")
  cat(
    "Self-links:      ",
    if (x$allow_self_links) "allowed" else "not allowed",
    "\n",
    sep = ""
  )
  
  if (isTRUE(x$has_guilds)) {
    cat("Guilds:          ")
    cat(paste(x$guilds, collapse = ", "))
    cat("\n")
  }
  
  if (length(x$diagnostics) > 0L) {
    cat("\nDiagnostics\n")
    cat("-----------\n")
    
    for (name in names(x$diagnostics)) {
      cat(name, ": ", x$diagnostics[[name]], "\n", sep = "")
    }
  }
  
  invisible(x)
}

#' Extract the standardized interaction table
#'
#' Extracts the interaction table stored inside a
#' `temporal_interactions` object.
#'
#' The returned data frame contains the standardized columns retained by
#' [prepare_interactions()]:
#'
#' * `Site`;
#' * `Time`;
#' * `Origin_node`;
#' * `Destiny_node`;
#' * optionally, `Origin_guild` and `Destiny_guild`; and
#' * `Strength`.
#'
#' For binary networks, `Strength` is represented internally by ones, although
#' the object's metadata continue to identify the network as binary.
#'
#' @param x A `temporal_interactions` object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return A data frame containing the standardized interaction table.
#'
#' @examples
#' interactions <- data.frame(
#'   Site = "site_1",
#'   Time = 2020,
#'   Origin_node = "plant_1",
#'   Destiny_node = "bee_1",
#'   Strength = 5
#' )
#'
#' prepared <- prepare_interactions(interactions)
#' as.data.frame(prepared)
#'
#' @method as.data.frame temporal_interactions
#' @rdname temporal_interactions
#' @export
as.data.frame.temporal_interactions <- function(
    x,
    ...
) {
  validate_temporal_interactions(x)
  x$interactions
}