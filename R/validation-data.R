# =============================================================================
# Validation of input interaction data
# =============================================================================

# -------------------------------------------------------------------------
# Validate the input object
# -------------------------------------------------------------------------
#' Validate the input data object
#'
#' Verifies that the supplied interaction data are provided as a non-empty
#' data frame.
#'
#' This function performs the first validation step of
#' [prepare_interactions()]. It does not inspect the contents of the data
#' frame; only its basic structure is checked.
#'
#' @param data An object expected to be a data frame.
#'
#' @return The validated data frame.
#'
#' @details
#' An error is raised if:
#'
#' * `data` is not a data frame; or
#' * `data` contains no rows.
#'
#' @keywords internal
validate_input_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  
  if (nrow(data) == 0L) {
    stop("`data` contains no rows.", call. = FALSE)
  }
  
  data
}

# -------------------------------------------------------------------------
# Validate mandatory columns
# -------------------------------------------------------------------------
#' Validate required interaction columns
#'
#' Checks that the minimum set of columns required by
#' [prepare_interactions()] is present.
#'
#' The following columns are mandatory:
#'
#' * `Site`
#' * `Time`
#' * `Origin_node`
#' * `Destiny_node`
#'
#' Optional columns such as guilds or interaction strength are validated
#' separately.
#'
#' @param data A data frame containing interaction records.
#'
#' @return The validated data frame.
#'
#' @details
#' An informative error listing all missing column names is produced whenever
#' one or more mandatory columns are absent.
#'
#' @keywords internal
validate_required_columns <- function(data) {
  required <- c(
    "Site",
    "Time",
    "Origin_node",
    "Destiny_node"
  )
  
  missing <- setdiff(required, names(data))
  
  if (length(missing) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  data
}


# -------------------------------------------------------------------------
# Validate interaction identifiers
# -------------------------------------------------------------------------
#' Validate interaction identifiers
#'
#' Checks that the identifiers required to describe each interaction are
#' complete.
#'
#' The following columns are inspected:
#'
#' * `Site`
#' * `Time`
#' * `Origin_node`
#' * `Destiny_node`
#'
#' Missing values (`NA`) and empty character strings are not allowed.
#'
#' @param data A data frame containing interaction records.
#'
#' @return The validated data frame.
#'
#' @details
#' Character values consisting only of white space are treated as empty.
#'
#' Validation is intentionally conservative: identifiers are not modified or
#' standardized automatically. The function simply verifies that every
#' interaction can be uniquely identified.
#'
#' @keywords internal
validate_identifiers <- function(data) {
  columns <- c(
    "Site",
    "Time",
    "Origin_node",
    "Destiny_node"
  )
  
  for (column in columns) {
    values <- data[[column]]
    
    invalid <- is.na(values) |
      trimws(as.character(values)) == ""
    
    if (any(invalid)) {
      stop(
        "`", column, "` contains ",
        sum(invalid),
        " missing or empty values.",
        call. = FALSE
      )
    }
  }
  
  data
}

# -------------------------------------------------------------------------
# Validate self-links
# -------------------------------------------------------------------------
#' Validate self-links
#'
#' Checks whether interactions connect a node to itself.
#'
#' Self-links are always forbidden in multipartite networks because a node
#' cannot simultaneously belong to different guilds.
#'
#' In unipartite networks, self-links are allowed only when
#' `allow_self_links = TRUE`.
#'
#' @param data A data frame containing `Origin_node` and `Destiny_node`.
#' @param allow_self_links Logical indicating whether self-links are permitted
#'   in unipartite networks.
#' @param topology Character string describing the network topology.
#'   Either `"unipartite"` or `"multipartite"`.
#'
#' @return The validated data frame.
#'
#' @details
#' A self-link is defined as an interaction for which
#' `Origin_node == Destiny_node`.
#'
#' @keywords internal
validate_self_links <- function(
    data,
    allow_self_links = FALSE,
    topology = "unipartite"
) {
  self_links <- as.character(data$Origin_node) ==
    as.character(data$Destiny_node)
  
  if (!any(self_links)) {
    return(data)
  }
  
  if (identical(topology, "multipartite")) {
    stop(
      "Self-links are not allowed in multipartite networks.",
      call. = FALSE
    )
  }
  
  if (!isTRUE(allow_self_links)) {
    stop(
      "Self-links were found, but `allow_self_links = FALSE`.",
      call. = FALSE
    )
  }
  
  data
}