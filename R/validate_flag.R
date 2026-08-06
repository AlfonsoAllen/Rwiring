# =============================================================================
# Validation of logical flags
# =============================================================================

#' Validate a logical flag
#'
#' Checks that an argument represents a valid logical flag.
#'
#' The function verifies that the supplied value is:
#'
#' * a logical vector;
#' * of length one;
#' * not missing (`NA`);
#' * or, optionally, `NULL`.
#'
#' This helper is used to validate logical arguments accepted by
#' [prepare_interactions()], such as `directed`, `multipartite`, and
#' `allow_self_links`.
#'
#' @param x The value to validate.
#' @param argument A character string containing the name of the argument.
#'   It is used to generate informative error messages.
#' @param allow_null Logical. If `TRUE`, `NULL` is accepted in addition to a
#'   single non-missing logical value.
#'
#' @return The validated value, invisibly.
#'
#' @details
#' The function is intended for internal use only. It standardizes the
#' validation of logical arguments across the package and ensures that error
#' messages remain consistent.
#'
#' @keywords internal
validate_flag <- function(x, argument, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  
  if (
    !is.logical(x) ||
    length(x) != 1L ||
    is.na(x)
  ) {
    stop(
      "`", argument, "` must be a single non-missing logical value",
      if (allow_null) " or NULL" else "",
      ".",
      call. = FALSE
    )
  }
  
  invisible(x)
}