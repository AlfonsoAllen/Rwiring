# =============================================================================
# Validation of link classification tables
# =============================================================================

#' Validate a link classification table
#'
#' Checks that a link classification table has the structure and contents
#' required by `quantify_network_changes()`.
#'
#' @param link_classification A data frame returned by
#'   [classify_network_links()].
#' @param site_1 Site identifying network 1.
#' @param time_1 Time identifying network 1.
#' @param site_2 Site identifying network 2.
#' @param time_2 Time identifying network 2.
#'
#' @return `link_classification`, invisibly, when valid.
#'
#' @keywords internal
validate_link_classification <- function(
    link_classification,
    site_1,
    time_1,
    site_2,
    time_2
) {
  if (!is.data.frame(link_classification)) {
    stop(
      "`link_classification` must be a data frame.",
      call. = FALSE
    )
  }
  
  if (nrow(link_classification) == 0L) {
    stop(
      "`link_classification` must contain at least one link.",
      call. = FALSE
    )
  }
  
  required <- c(
    "Site_1",
    "Time_1",
    "Site_2",
    "Time_2",
    "Comparison_type",
    "Origin_node",
    "Destiny_node",
    "Origin_status",
    "Destiny_status",
    "Present_network_1",
    "Present_network_2",
    "Strength_network_1",
    "Strength_network_2",
    "Direction",
    "Mechanism"
  )
  
  missing <- setdiff(
    required,
    names(link_classification)
  )
  
  if (length(missing) > 0L) {
    stop(
      "`link_classification` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  valid_directions <- c(
    "persistent",
    "lost",
    "gained"
  )
  
  valid_mechanisms <- c(
    "permanent",
    "rewiring",
    "one_non_permanent_node",
    "two_non_permanent_nodes"
  )
  
  if (
    anyNA(link_classification$Direction) ||
    !all(link_classification$Direction %in% valid_directions)
  ) {
    stop(
      "`link_classification$Direction` contains invalid values.",
      call. = FALSE
    )
  }
  
  if (
    anyNA(link_classification$Mechanism) ||
    !all(link_classification$Mechanism %in% valid_mechanisms)
  ) {
    stop(
      "`link_classification$Mechanism` contains invalid values.",
      call. = FALSE
    )
  }
  
  invalid_origin <-
    is.na(link_classification$Origin_node) |
    trimws(as.character(
      link_classification$Origin_node
    )) == ""
  
  invalid_destiny <-
    is.na(link_classification$Destiny_node) |
    trimws(as.character(
      link_classification$Destiny_node
    )) == ""
  
  if (any(invalid_origin | invalid_destiny)) {
    stop(
      "`link_classification` contains missing or empty node identifiers.",
      call. = FALSE
    )
  }
  
  status_columns <- c(
    "Origin_status",
    "Destiny_status"
  )
  
  valid_status <- c(
    "permanent",
    "lost",
    "gained"
  )
  
  for (column in status_columns) {
    if (
      anyNA(link_classification[[column]]) ||
      !all(link_classification[[column]] %in% valid_status)
    ) {
      stop(
        "`", column, "` contains invalid node classifications.",
        call. = FALSE
      )
    }
  }
  
  same_comparison <-
    isTRUE(all(link_classification$Site_1 == site_1)) &&
    isTRUE(all(link_classification$Time_1 == time_1)) &&
    isTRUE(all(link_classification$Site_2 == site_2)) &&
    isTRUE(all(link_classification$Time_2 == time_2))
  
  if (!isTRUE(same_comparison)) {
    stop(
      "`link_classification` does not correspond to the requested ",
      "network comparison.",
      call. = FALSE
    )
  }
  
  presence_columns <- c(
    "Present_network_1",
    "Present_network_2"
  )
  
  for (column in presence_columns) {
    if (
      !is.logical(link_classification[[column]]) ||
      anyNA(link_classification[[column]])
    ) {
      stop(
        "`", column,
        "` must contain non-missing logical values.",
        call. = FALSE
      )
    }
  }
  
  if (any(
    !link_classification$Present_network_1 &
    !link_classification$Present_network_2
  )) {
    stop(
      "Every classified link must occur in at least one network.",
      call. = FALSE
    )
  }
  
  guild_columns <- c(
    "Origin_guild",
    "Destiny_guild"
  )
  
  guild_columns_present <-
    guild_columns %in% names(link_classification)
  
  if (any(guild_columns_present) && !all(guild_columns_present)) {
    stop(
      "`Origin_guild` and `Destiny_guild` must be supplied together.",
      call. = FALSE
    )
  }
  
  
  link_keys <- c(
    "Origin_node",
    "Destiny_node"
  )
  
  if (all(guild_columns_present)) {
    link_keys <- c(
      link_keys,
      guild_columns
    )
  }
  
  if (anyDuplicated(link_classification[link_keys])) {
    stop(
      "`link_classification` must contain one row per link.",
      call. = FALSE
    )
  }
  
  
  strength_columns <- c(
    "Strength_network_1",
    "Strength_network_2"
  )
  
  for (column in strength_columns) {
    values <- link_classification[[column]]
    
    if (
      !is.numeric(values) ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      any(values < 0)
    ) {
      stop(
        "`", column,
        "` must contain finite, non-negative numeric values.",
        call. = FALSE
      )
    }
  }
  
  
  invalid_absent_strength <-
    (
      !link_classification$Present_network_1 &
        link_classification$Strength_network_1 != 0
    ) |
    (
      !link_classification$Present_network_2 &
        link_classification$Strength_network_2 != 0
    )
  
  if (any(invalid_absent_strength)) {
    stop(
      "An absent link must have strength zero in the corresponding network.",
      call. = FALSE
    )
  }
  
  
  expected_direction <- ifelse(
    link_classification$Present_network_1 &
      link_classification$Present_network_2,
    "persistent",
    ifelse(
      link_classification$Present_network_1,
      "lost",
      "gained"
    )
  )
  
  invalid <- which(
    as.character(link_classification$Direction) !=
      expected_direction
  )
  
  if (length(invalid) > 0L) {
    stop(
      "`Direction` is inconsistent with the presence indicators ",
      "for row ", invalid[1], ".",
      call. = FALSE
    )
  }
  
  persistent_links <-
    link_classification$Direction == "persistent"
  
  lost_links <-
    link_classification$Direction == "lost"
  
  gained_links <-
    link_classification$Direction == "gained"
  
  if (
    any(
      link_classification$Origin_status[persistent_links] !=
      "permanent"
    ) ||
    any(
      link_classification$Destiny_status[persistent_links] !=
      "permanent"
    )
  ) {
    stop(
      "Persistent links must connect two permanent nodes.",
      call. = FALSE
    )
  }
  
  if (
    any(
      link_classification$Origin_status[lost_links] == "gained"
    ) ||
    any(
      link_classification$Destiny_status[lost_links] == "gained"
    )
  ) {
    stop(
      "Lost links cannot contain nodes classified as gained.",
      call. = FALSE
    )
  }
  
  if (
    any(
      link_classification$Origin_status[gained_links] == "lost"
    ) ||
    any(
      link_classification$Destiny_status[gained_links] == "lost"
    )
  ) {
    stop(
      "Gained links cannot contain nodes classified as lost.",
      call. = FALSE
    )
  }
  
  n_permanent_nodes <-
    (link_classification$Origin_status == "permanent") +
    (link_classification$Destiny_status == "permanent")
  
  expected_mechanism <- ifelse(
    link_classification$Direction == "persistent",
    "permanent",
    ifelse(
      n_permanent_nodes == 2L,
      "rewiring",
      ifelse(
        n_permanent_nodes == 1L,
        "one_non_permanent_node",
        "two_non_permanent_nodes"
      )
    )
  )
  
  if (
    !identical(
      as.character(link_classification$Mechanism),
      expected_mechanism
    )
  ) {
    stop(
      "`Mechanism` is inconsistent with `Direction` and the node statuses.",
      call. = FALSE
    )
  }
  
  expected_comparison_type <- if (
    isTRUE(site_1 == site_2) &&
    !isTRUE(time_1 == time_2)
  ) {
    "temporal"
  } else if (
    !isTRUE(site_1 == site_2) &&
    isTRUE(time_1 == time_2)
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
    anyNA(link_classification$Comparison_type) ||
    !all(
      link_classification$Comparison_type %in%
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
      link_classification$Comparison_type ==
      expected_comparison_type
    ))
  ) {
    stop(
      "`Comparison_type` is inconsistent with the requested comparison.",
      call. = FALSE
    )
  }
  
  invisible(link_classification)
}