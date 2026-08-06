# =============================================================================
# Validation and processing of interaction strengths
# =============================================================================

#' Process interaction-strength information
#'
#' Validates the `Strength` column and determines whether the interaction data
#' should be treated as weighted or binary.
#'
#' If the `Strength` column is absent or entirely missing, all interactions are
#' interpreted as binary presences and assigned `Strength = 1`.
#'
#' When `Strength` is only partially missing, the behaviour is controlled by
#' `missing_strength`.
#'
#' @param data A data frame containing interaction records. The `Strength`
#'   column is optional.
#' @param missing_strength Character string controlling how partially missing
#'   strength values are handled. One of:
#'
#'   * `"error"`: stop with an error.
#'   * `"binary"`: discard all supplied strengths and treat every interaction
#'     as a binary presence with `Strength = 1`.
#'   * `"warn_binary"`: as `"binary"`, but issue a warning.
#'
#' @return A named list containing:
#'
#' * `data`: the interaction table with a complete `Strength` column.
#' * `weighted`: logical indicating whether the network should be treated as
#'   weighted.
#' * `binary_conversion`: character value describing why the data were
#'   converted to binary, or `NULL` when the original strengths were retained.
#'
#' Possible values of `binary_conversion` are:
#'
#' * `"missing_column"`
#' * `"all_missing"`
#' * `"partial_missing"`
#'
#' @details
#' When strength information is complete, `Strength` must be numeric, finite,
#' and non-negative.
#'
#' Binary networks are represented internally by setting `Strength = 1` for
#' every interaction. The returned `weighted` flag preserves the distinction
#' between binary and weighted data.
#'
#' Zero-strength values are permitted at this stage and are processed
#' separately by `handle_zero_strength()`.
#'
#' @keywords internal
process_strength_information <- function(
    data,
    missing_strength = c("error", "binary", "warn_binary")
) {
  missing_strength <- match.arg(missing_strength)
  
  if (!"Strength" %in% names(data)) {
    data$Strength <- 1
    
    return(list(
      data = data,
      weighted = FALSE,
      binary_conversion = "missing_column"
    ))
  }
  
  all_missing <- all(is.na(data$Strength))
  partially_missing <- anyNA(data$Strength) && !all_missing
  
  if (all_missing) {
    data$Strength <- 1
    
    return(list(
      data = data,
      weighted = FALSE,
      binary_conversion = "all_missing"
    ))
  }
  
  if (partially_missing) {
    if (missing_strength == "error") {
      stop(
        "`Strength` is partially missing.",
        call. = FALSE
      )
    }
    
    if (missing_strength == "warn_binary") {
      warning(
        "`Strength` is partially missing. Existing weights ",
        "will be discarded and all interactions treated as binary.",
        call. = FALSE
      )
    }
    
    data$Strength <- 1
    
    return(list(
      data = data,
      weighted = FALSE,
      binary_conversion = "partial_missing"
    ))
  }
  
  if (!is.numeric(data$Strength)) {
    stop("`Strength` must be numeric.", call. = FALSE)
  }
  
  if (any(!is.finite(data$Strength))) {
    stop(
      "`Strength` must contain only finite values.",
      call. = FALSE
    )
  }
  
  if (any(data$Strength < 0)) {
    stop(
      "`Strength` cannot contain negative values.",
      call. = FALSE
    )
  }
  
  list(
    data = data,
    weighted = TRUE,
    binary_conversion = NULL
  )
}

#' Handle zero-strength interactions
#'
#' Processes rows whose interaction strength is equal to zero.
#'
#' In an edge-list representation, rows with `Strength = 0` generally
#' correspond to absent rather than observed interactions. The function can
#' remove them, retain them, or raise an error.
#'
#' @param data A data frame containing a numeric `Strength` column.
#' @param action Character string controlling how zero-strength rows are
#'   handled. One of:
#'
#'   * `"remove"`: remove zero-strength rows and issue a warning.
#'   * `"error"`: stop if any zero-strength rows are detected.
#'   * `"keep"`: retain zero-strength rows and issue a warning.
#'
#' @return A named list containing:
#'
#' * `data`: the processed interaction table.
#' * `n_zero_removed`: number of zero-strength rows removed.
#' * `zero_action`: the action applied.
#'
#' @details
#' If `action = "remove"` and all rows have zero strength, an error is raised
#' because no active interactions remain.
#'
#' When `action = "keep"`, `n_zero_removed` is zero because the rows remain in
#' the interaction table.
#'
#' @keywords internal
handle_zero_strength <- function(
    data,
    action = c("remove", "error", "keep")
) {
  action <- match.arg(action)
  
  zero_rows <- data$Strength == 0
  n_zero <- sum(zero_rows)
  
  if (n_zero == 0L) {
    return(list(
      data = data,
      n_zero_removed = 0L,
      zero_action = action
    ))
  }
  
  if (action == "error") {
    stop(
      "`Strength` contains ",
      n_zero,
      " zero values. In an edge list, zero-strength rows ",
      "represent absent interactions.",
      call. = FALSE
    )
  }
  
  if (action == "keep") {
    warning(
      n_zero,
      " zero-strength interactions were retained. ",
      "They will be treated as rows in the edge list but do not ",
      "represent active interactions.",
      call. = FALSE
    )
    
    return(list(
      data = data,
      n_zero_removed = 0L,
      zero_action = action
    ))
  }
  
  warning(
    n_zero,
    " zero-strength interactions were removed because they ",
    "represent absent links.",
    call. = FALSE
  )
  
  filtered_data <- data[!zero_rows, , drop = FALSE]
  
  if (nrow(filtered_data) == 0L) {
    stop(
      "All interactions have zero strength; no active links remain.",
      call. = FALSE
    )
  }
  
  list(
    data = filtered_data,
    n_zero_removed = n_zero,
    zero_action = action
  )
}