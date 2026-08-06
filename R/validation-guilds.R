# =============================================================================
# Validation and processing of guild information
# =============================================================================

# -------------------------------------------------------------------------
# Process guild information
# -------------------------------------------------------------------------
#' Process guild information
#'
#' Validates the guild information associated with interaction data and
#' determines the network topology.
#'
#' The function verifies that guild information is internally consistent and
#' returns metadata describing whether the network should be treated as
#' unipartite or multipartite.
#'
#' Guild information is optional. When guild columns are absent, the network
#' is treated as unipartite unless `multipartite = TRUE`, in which case an
#' error is raised.
#'
#' @param data A data frame containing standardized interaction records.
#' @param multipartite Logical or `NULL`.
#'
#'   * `TRUE` requires the interaction data to satisfy a multipartite
#'     structure.
#'   * `FALSE` forces the network to be treated as unipartite.
#'   * `NULL` infers the topology from the supplied guild information.
#'
#' @return A named list containing:
#'
#' * `data`: the validated interaction table.
#' * `has_guilds`: logical indicating whether guild information was supplied.
#' * `topology`: `"unipartite"` or `"multipartite"`.
#' * `n_guilds`: number of distinct guilds, or `NA_integer_` when guild
#'   information is absent.
#' * `guilds`: character vector containing the guild names, or `NULL`.
#'
#' @details
#' The function performs the following checks:
#'
#' * both guild columns are either simultaneously present or absent;
#' * guild assignments are complete (no missing or empty values);
#' * every node belongs to a single guild;
#' * multipartite networks contain only interactions between different guilds.
#'
#' When `multipartite = NULL`, the topology is inferred automatically.
#' A network is classified as multipartite when at least two guilds are
#' present and every interaction connects different guilds.
#'
#' @keywords internal

process_guild_information <- function(
    data,
    multipartite = NULL
) {
  guild_columns <- c(
    "Origin_guild",
    "Destiny_guild"
  )
  
  present <- guild_columns %in% names(data)
  
  if (any(present) && !all(present)) {
    stop(
      "Both `Origin_guild` and `Destiny_guild` must be supplied.",
      call. = FALSE
    )
  }
  
  if (!all(present)) {
    if (isTRUE(multipartite)) {
      stop(
        "`multipartite = TRUE` requires guild information.",
        call. = FALSE
      )
    }
    
    return(list(
      data = data,
      has_guilds = FALSE,
      topology = "unipartite",
      n_guilds = NA_integer_, #no conocemos el número real de guilds, porque el usuario no proporcionó esa información
      guilds = NULL
    ))
  }
  
  guild_missing <-
    is.na(data$Origin_guild) |
    is.na(data$Destiny_guild) |
    trimws(as.character(data$Origin_guild)) == "" |
    trimws(as.character(data$Destiny_guild)) == ""
  
  if (any(guild_missing)) {
    stop(
      "Guild information is partially missing in ",
      sum(guild_missing),
      " rows.",
      call. = FALSE
    )
  }
  
  node_guilds <- validate_node_guild_consistency(data)
  
  guilds <- sort(unique(node_guilds$Guild))
  n_guilds <- length(guilds)
  
  between_guild_links <-
    as.character(data$Origin_guild) !=
    as.character(data$Destiny_guild)
  
  inferred_multipartite <-
    n_guilds >= 2L && all(between_guild_links)
  
  if (isTRUE(multipartite) && !inferred_multipartite) {
    stop(
      "The data do not satisfy a multipartite structure: ",
      "at least one interaction connects nodes from the same guild.",
      call. = FALSE
    )
  }
  
  topology <- if (
    identical(multipartite, TRUE) ||
    is.null(multipartite) && inferred_multipartite
  ) {
    "multipartite"
  } else {
    "unipartite"
  }
  
  list(
    data = data,
    has_guilds = TRUE,
    topology = topology,
    n_guilds = n_guilds,
    guilds = guilds
  )
}

# -------------------------------------------------------------------------
# Validate node-guild assignments
# -------------------------------------------------------------------------
#' Validate node-guild assignments
#'
#' Verifies that every node is assigned to a unique guild throughout the
#' interaction data.
#'
#' The function combines origin and destination node assignments and checks
#' that no node appears associated with more than one guild.
#'
#' @param data A data frame containing the columns `Origin_node`,
#'   `Destiny_node`, `Origin_guild`, and `Destiny_guild`.
#'
#' @return A data frame with two columns:
#'
#' * `Node`
#' * `Guild`
#'
#' containing one row for each unique node-guild association.
#'
#' @details
#' An error is raised whenever a node is assigned to more than one guild.
#'
#' The returned table is subsequently used to determine the list of guilds
#' present in the interaction data.
#'
#' @keywords internal

validate_node_guild_consistency <- function(data) {
  origin <- data.frame(
    Node = as.character(data$Origin_node),
    Guild = as.character(data$Origin_guild),
    stringsAsFactors = FALSE
  )
  
  destiny <- data.frame(
    Node = as.character(data$Destiny_node),
    Guild = as.character(data$Destiny_guild),
    stringsAsFactors = FALSE
  )
  
  node_guilds <- unique(rbind(origin, destiny))
  
  guild_count <- stats::aggregate(
    Guild ~ Node,
    data = node_guilds,
    FUN = function(x) length(unique(x))
  )
  
  inconsistent <- guild_count$Node[guild_count$Guild > 1L]
  
  if (length(inconsistent) > 0L) {
    stop(
      "The following nodes are assigned to more than one guild: ",
      paste(inconsistent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  node_guilds
}