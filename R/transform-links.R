# =============================================================================
# Transformation of interaction links
# =============================================================================

# -------------------------------------------------------------------------
# Canonicalize undirected links
# -------------------------------------------------------------------------
#' Canonicalize undirected interaction links
#'
#' Standardizes the orientation of links in undirected networks so that each
#' unordered pair of nodes is represented consistently.
#'
#' When `directed = FALSE`, node identifiers are converted to character and
#' ordered alphabetically. If `Origin_node` is alphabetically greater than
#' `Destiny_node`, the two node identifiers are exchanged. When guild columns
#' are present, their values are exchanged together with the corresponding
#' nodes.
#'
#' Directed networks are returned unchanged.
#'
#' @param data A data frame containing at least `Origin_node` and
#'   `Destiny_node`. It may also contain `Origin_guild` and `Destiny_guild`.
#' @param directed A single logical value indicating whether the network is
#'   directed.
#'
#' @return A data frame with canonically ordered node pairs when
#'   `directed = FALSE`. The input data are returned unchanged when
#'   `directed = TRUE`.
#'
#' @details
#' Canonicalization allows links recorded in opposite orientations, such as
#' `A-B` and `B-A`, to be recognized as the same interaction before duplicate
#' links are aggregated.
#'
#' Node and guild columns are converted to character only for undirected
#' networks. This avoids invalid factor-level assignments when node positions
#' are exchanged.
#'
#' @keywords internal
canonicalize_links <- function(data, directed) {
  if (isTRUE(directed)) {
    return(data)
  }
  
  data$Origin_node <- as.character(data$Origin_node)
  data$Destiny_node <- as.character(data$Destiny_node)
  
  guild_columns <- c("Origin_guild", "Destiny_guild")
  has_guilds <- all(guild_columns %in% names(data))
  
  if (has_guilds) {
    data$Origin_guild <- as.character(data$Origin_guild)
    data$Destiny_guild <- as.character(data$Destiny_guild)
  }
  
  swap <- data$Origin_node > data$Destiny_node
  
  if (!any(swap)) {
    return(data)
  }
  
  origin_node <- data$Origin_node[swap]
  data$Origin_node[swap] <- data$Destiny_node[swap]
  data$Destiny_node[swap] <- origin_node
  
  if (has_guilds) {
    origin_guild <- data$Origin_guild[swap]
    data$Origin_guild[swap] <- data$Destiny_guild[swap]
    data$Destiny_guild[swap] <- origin_guild
  }
  
  data
}

# -------------------------------------------------------------------------
# Aggregate duplicated links
# -------------------------------------------------------------------------
#' Aggregate duplicated interaction links
#'
#' Identifies repeated links observed within the same site and time point and
#' either aggregates them or raises an error.
#'
#' Duplicate links are defined using the combination of `Site`, `Time`,
#' `Origin_node`, and `Destiny_node`. When guild columns are present,
#' `Origin_guild` and `Destiny_guild` are also included in the grouping keys.
#'
#' @param data A standardized interaction data frame containing `Site`, `Time`,
#'   `Origin_node`, `Destiny_node`, and `Strength`. Guild columns may also be
#'   present.
#' @param weighted A single logical value indicating whether `Strength`
#'   represents quantitative interaction weights.
#' @param action Character string controlling how duplicated links are handled.
#'   One of:
#'
#'   * `"aggregate"`: duplicated strengths are summed in weighted networks.
#'     In binary networks, duplicated observations are collapsed into a single
#'     interaction with `Strength = 1`.
#'   * `"error"`: stop if duplicated links are detected.
#'
#' @return A data frame in which duplicated links have either been aggregated
#'   or left unchanged when no duplicates are present.
#'
#' @details
#' In weighted networks, aggregation is performed by summing `Strength` within
#' each unique site-time-link combination.
#'
#' In binary networks, repeated rows do not increase interaction strength.
#' They are reduced to a single presence record.
#'
#' This function assumes that undirected links have already been canonicalized
#' with `canonicalize_links()`.
#'
#' @keywords internal
aggregate_duplicate_links <- function(
    data,
    weighted,
    action = c("aggregate", "error")
) {
  action <- match.arg(action)
  
  keys <- c(
    "Site",
    "Time",
    "Origin_node",
    "Destiny_node"
  )
  
  if (
    "Origin_guild" %in% names(data) &&
    "Destiny_guild" %in% names(data)
  ) {
    keys <- c(
      keys,
      "Origin_guild",
      "Destiny_guild"
    )
  }
  
  duplicated_link <- duplicated(data[keys]) |
    duplicated(data[keys], fromLast = TRUE)
  
  if (!any(duplicated_link)) {
    return(data)
  }
  
  if (action == "error") {
    stop(
      "Duplicated links were found within the same Site and Time.",
      call. = FALSE
    )
  }
  
  if (weighted) {
    stats::aggregate(
      data$Strength,
      by = data[keys],
      FUN = sum,
      na.rm = FALSE
    ) |>
      stats::setNames(c(keys, "Strength"))
  } else {
    unique(data[c(keys, "Strength")])
  }
}