# =============================================================================
# Preparation and validation of temporal interaction data
# =============================================================================

#' Prepare and validate temporal interaction data
#'
#' Validates and standardizes an edge list describing interactions observed
#' across sites and time points. The function supports binary or weighted,
#' directed or undirected, and unipartite or multipartite networks.
#'
#' @param data A data frame containing one row per observed interaction.
#'   The following columns are required:
#'
#'   * `Site`: site or location where the interaction was observed.
#'   * `Time`: time point at which the interaction was observed.
#'   * `Origin_node`: identifier of the node from which the interaction starts.
#'   * `Destiny_node`: identifier of the node receiving the interaction.
#'
#'   The following columns are optional:
#'
#'   * `Origin_guild`: guild assigned to the origin node.
#'   * `Destiny_guild`: guild assigned to the destination node.
#'   * `Strength`: non-negative numeric strength of the interaction.
#'
#'   If guild information is supplied, both guild columns must be present and
#'   completely filled. Each node must be assigned consistently to a single
#'   guild throughout the data set.
#'
#' @param directed Logical. Should interactions be treated as directed?
#'   If `TRUE`, interactions from `A` to `B` and from `B` to `A` are treated
#'   as different links. If `FALSE`, both orientations represent the same
#'   interaction and node order is canonicalized before duplicated links are
#'   identified. Defaults to `TRUE`.
#'
#' @param multipartite Logical or `NULL`. Determines whether the network must
#'   satisfy a multipartite structure.
#'
#'   * `TRUE`: guild information is required and every interaction must connect
#'     nodes belonging to different guilds.
#'   * `FALSE`: the network is treated as unipartite, even if guild information
#'     is supplied.
#'   * `NULL`: the topology is inferred from the data. The network is classified
#'     as multipartite when at least two guilds are present and all interactions
#'     occur between different guilds. If guild information is absent, the
#'     network is treated as unipartite.
#'
#' @param missing_strength Character string controlling how partially missing
#'   values in `Strength` are handled. One of:
#'
#'   * `"error"`: stop with an error.
#'   * `"binary"`: discard all supplied strengths and treat every interaction
#'     as a binary presence with `Strength = 1`.
#'   * `"warn_binary"`: as `"binary"`, but issue a warning.
#'
#'   If the `Strength` column is absent or entirely missing, the data are
#'   automatically treated as binary. Defaults to `"error"`.
#'
#' @param duplicate_links Character string controlling duplicated links within
#'   the same `Site` and `Time`. One of:
#'
#'   * `"aggregate"`: sum duplicated strengths in weighted networks, or collapse
#'     duplicated binary observations into one interaction with
#'     `Strength = 1`.
#'   * `"error"`: stop if duplicated links are detected.
#'
#'   Defaults to `"aggregate"`.
#'
#' @param zero_strength Character string controlling interactions with
#'   `Strength = 0`. One of:
#'
#'   * `"remove"`: remove zero-strength rows and issue a warning.
#'   * `"error"`: stop if zero-strength rows are present.
#'   * `"keep"`: retain zero-strength rows and issue a warning.
#'
#'   Defaults to `"remove"`.
#'
#' @param allow_self_links Logical. Whether links connecting a node to itself
#'   are allowed in unipartite networks. Self-links are never allowed in
#'   multipartite networks; in that case, the stored network metadata record
#'   `allow_self_links = FALSE`. Defaults to `FALSE`.
#'
#' @return An object of class `temporal_interactions`. The object contains:
#'
#'   * `interactions`: the standardized interaction table.
#'   * `network_type`: metadata describing whether the network is directed,
#'     weighted, unipartite or multipartite, whether self-links are allowed,
#'     and the available guild information.
#'   * `diagnostics`: information about removed zero-strength rows, aggregated
#'     duplicates, binary conversion, and the number of input and output rows.
#'
#' @details
#' The function validates the structure of the interaction data before temporal
#' comparisons are performed. Missing or empty values are not allowed in
#' `Site`, `Time`, `Origin_node`, or `Destiny_node`.
#'
#' When guild columns are supplied, guild assignments must be complete and
#' consistent across both node columns. A node appearing as an origin in one
#' row and as a destination in another must retain the same guild assignment.
#'
#' A supplied `Strength` column must be numeric, finite, and non-negative.
#' Binary networks are internally represented using a `Strength` column equal
#' to one, while the object's metadata retain the distinction between binary
#' and weighted data.
#'
#' For undirected networks, node order is standardized so that reverse
#' observations of the same interaction can be recognized as duplicates.
#'
#' Only the standardized columns used by the package are retained in the
#' resulting interaction table. Additional columns in `data` are discarded.
#'
#' @examples
#' interactions <- data.frame(
#'   Site = c("site_1", "site_1", "site_1", "site_1"),
#'   Time = c(2020, 2020, 2021, 2021),
#'   Origin_node = c("plant_1", "plant_2", "plant_1", "plant_2"),
#'   Destiny_node = c("bee_1", "bee_2", "bee_1", "bee_2"),
#'   Origin_guild = c("plant", "plant", "plant", "plant"),
#'   Destiny_guild = c(
#'     "pollinator", "pollinator", "pollinator", "pollinator"
#'   ),
#'   Strength = c(5, 2, 7, 4)
#' )
#'
#' prepared <- prepare_interactions(
#'   interactions,
#'   directed = FALSE,
#'   multipartite = TRUE
#' )
#'
#' prepared
#' summary(prepared)
#' as.data.frame(prepared)
#'
#' # Binary data can be supplied without a Strength column
#' binary_interactions <- interactions[
#'   c(
#'     "Site",
#'     "Time",
#'     "Origin_node",
#'     "Destiny_node",
#'     "Origin_guild",
#'     "Destiny_guild"
#'   )
#' ]
#'
#' binary_prepared <- prepare_interactions(
#'   binary_interactions,
#'   directed = FALSE,
#'   multipartite = TRUE
#' )
#'
#' @export
prepare_interactions <- function(
    data,
    directed = TRUE,
    multipartite = NULL,
    missing_strength = c("error", "binary", "warn_binary"),
    duplicate_links = c("aggregate", "error"),
    zero_strength = c("remove", "error", "keep"),
    allow_self_links = FALSE
) {
  missing_strength <- match.arg(missing_strength)
  duplicate_links <- match.arg(duplicate_links)
  zero_strength <- match.arg(zero_strength)
  
  validate_flag(directed, "directed")
  validate_flag(multipartite, "multipartite", allow_null = TRUE)
  validate_flag(allow_self_links, "allow_self_links")
  
  data <- validate_input_data(data)
  n_rows_input <- nrow(data)
  
  data <- validate_required_columns(data)
  data <- validate_identifiers(data)
  
  guild_info <- process_guild_information(
    data = data,
    multipartite = multipartite
  )
  
  data <- guild_info$data
  
  data <- validate_self_links(
    data = data,
    allow_self_links = allow_self_links,
    topology = guild_info$topology
  )
  
  strength_info <- process_strength_information(
    data = data,
    missing_strength = missing_strength
  )
  
  data <- strength_info$data
  
  zero_info <- handle_zero_strength(
    data = data,
    action = zero_strength
  )
  
  data <- zero_info$data
  
  data <- canonicalize_links(
    data = data,
    directed = directed
  )
  
  standard_columns <- c(
    "Site",
    "Time",
    "Origin_node",
    "Destiny_node",
    "Origin_guild",
    "Destiny_guild",
    "Strength"
  )
  
  data <- data[
    intersect(standard_columns, names(data))
  ]
  
  n_rows_before_duplicates <- nrow(data)
  
  data <- aggregate_duplicate_links(
    data = data,
    weighted = strength_info$weighted,
    action = duplicate_links
  )
  
  diagnostics <- list(
    rows_input = n_rows_input,
    rows_output = nrow(data),
    zero_strength_rows_removed = zero_info$n_zero_removed,
    duplicate_rows_aggregated =
      n_rows_before_duplicates - nrow(data),
    binary_conversion = if (
      is.null(strength_info$binary_conversion)
    ) {
      "none"
    } else {
      strength_info$binary_conversion
    }
  )
  
  new_temporal_interactions(
    interactions = data,
    directed = directed,
    topology = guild_info$topology,
    weighted = strength_info$weighted,
    allow_self_links = isTRUE(allow_self_links) &&
      guild_info$topology == "unipartite",
    has_guilds = guild_info$has_guilds,
    n_guilds = guild_info$n_guilds,
    guilds = guild_info$guilds,
    diagnostics = diagnostics
  )
}