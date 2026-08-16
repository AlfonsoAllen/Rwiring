#' Plot a matrix of network changes
#'
#' Visualizes changes between two interaction networks using the matrix
#' representation created internally by [build_network_change_matrix()].
#'
#' Interaction persistence, loss, and gain are represented by cell colour.
#' For persistent interactions in weighted networks, changes in interaction
#' strength can additionally be indicated by upward and downward triangles.
#' Interaction strength can also be optionally encoded by cell opacity.
#'
#' @param x A `network_changes` object created by
#'   [quantify_network_changes()].
#' @param row_guild Optional character value specifying the guild to place on
#'   the rows of an undirected two-guild multipartite interaction matrix.
#'   If `NULL`, guild names are ordered alphabetically and the first guild is
#'   assigned to rows. Ignored for adjacency matrices.
#' @param show_node_labels Logical. If `TRUE`, node names are displayed on
#'   the matrix axes. Defaults to `TRUE`.
#' @param show_strength_symbols Logical. If `TRUE`, persistent interactions
#'   classified as strengthened or weakened are marked with upward and
#'   downward triangles, respectively. Persistent interactions classified as
#'   unchanged are shown without a symbol. Defaults to `TRUE`.
#' @param encode_strength Logical. If `TRUE`, cell opacity is scaled according
#'   to interaction strength. Persistent links use the mean strength across
#'   both networks, lost links use their strength in network 1, and gained
#'   links use their strength in network 2. This option is available only for
#'   weighted networks. Defaults to `FALSE`.
#' @param strength_max Optional positive numeric value defining the interaction
#'   strength represented with maximum opacity when `encode_strength = TRUE`.
#'   If `NULL`, the maximum plotted strength in the current comparison is
#'   used. Values exceeding `strength_max` are displayed at maximum opacity.
#'   Supplying the same value across plots allows a common opacity scale to be
#'   used when comparing multiple network changes.
#' @param min_alpha A numeric value between 0 and 1 specifying the minimum
#'   cell opacity used when `encode_strength = TRUE`. Defaults to `0.2`.
#'
#' @return A `ggplot` object representing the network comparison.
#'
#' @details
#' The matrix geometry is determined automatically from the network topology
#' and direction:
#'
#' * undirected multipartite networks with exactly two guilds are represented
#'   as rectangular interaction matrices;
#' * unipartite networks are represented as square adjacency matrices;
#' * directed multipartite networks and multipartite networks with more than
#'   two guilds are represented as square adjacency matrices.
#'
#' Nodes are ordered according to their classification in the network
#' comparison. Permanent nodes are shown first, followed by lost and gained
#' nodes, with alphabetical ordering within each status group. For
#' multipartite adjacency matrices, guilds form separate alphabetical blocks
#' and node-status ordering is applied within each guild.
#'
#' Cell colour represents interaction change:
#'
#' * `"persistent"`: interaction present in both networks;
#' * `"lost"`: interaction present only in network 1;
#' * `"gained"`: interaction present only in network 2.
#'
#' The default colours use grey for persistent interactions, orange for lost
#' interactions, and blue for gained interactions.
#'
#' Strength-change symbols are used only for persistent interactions because
#' the strength direction of lost and gained interactions is already implied
#' by their structural change. Persistent interactions are represented as:
#'
#' * `"strengthened"`: upward triangle;
#' * `"weakened"`: downward triangle;
#' * `"unchanged"`: no symbol.
#'
#' Thus, lost interactions are displayed only by their lost-link colour and
#' gained interactions only by their gained-link colour, without redundant
#' strength-direction symbols.
#'
#' When `encode_strength = TRUE`, opacity represents interaction magnitude
#' using
#'
#' \deqn{
#' s =
#' \begin{cases}
#' (w_1 + w_2)/2, & \text{persistent links},\\
#' w_1, & \text{lost links},\\
#' w_2, & \text{gained links},
#' \end{cases}
#' }
#'
#' where \eqn{w_1} and \eqn{w_2} are the interaction strengths in networks 1
#' and 2, respectively. Opacity is scaled between `min_alpha` and 1. When
#' `strength_max` is supplied, strengths equal to or greater than this value
#' are displayed at maximum opacity.
#'
#' Thin lines separate permanent, lost, and gained node groups. For
#' multipartite adjacency matrices, thicker lines additionally separate guild
#' blocks. A border is drawn around the complete matrix to delimit its extent,
#' including when node labels are hidden.
#'
#' @seealso [quantify_network_changes()]
#'
#' @export
plot_network_changes_matrix <- function(
    x,
    row_guild = NULL,
    show_node_labels = TRUE,
    show_strength_symbols = TRUE,
    encode_strength = FALSE,
    strength_max = NULL,
    min_alpha = 0.2
) {
  
  # -----------------------------------------------------------------------
  # Validate arguments
  # -----------------------------------------------------------------------
  
  validate_network_changes(x)
  
  if (
    !is.logical(show_node_labels) ||
    length(show_node_labels) != 1L ||
    is.na(show_node_labels)
  ) {
    stop(
      "`show_node_labels` must be a single non-missing logical value.",
      call. = FALSE
    )
  }
  
  if (
    !is.logical(show_strength_symbols) ||
    length(show_strength_symbols) != 1L ||
    is.na(show_strength_symbols)
  ) {
    stop(
      "`show_strength_symbols` must be a single non-missing logical value.",
      call. = FALSE
    )
  }
  
  if (
    !is.logical(encode_strength) ||
    length(encode_strength) != 1L ||
    is.na(encode_strength)
  ) {
    stop(
      "`encode_strength` must be a single non-missing logical value.",
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(min_alpha) ||
    length(min_alpha) != 1L ||
    is.na(min_alpha) ||
    !is.finite(min_alpha) ||
    min_alpha < 0 ||
    min_alpha > 1
  ) {
    stop(
      "`min_alpha` must be a single finite numeric value between 0 and 1.",
      call. = FALSE
    )
  }
  
  if (
    !is.null(strength_max) &&
    (
      !is.numeric(strength_max) ||
      length(strength_max) != 1L ||
      is.na(strength_max) ||
      !is.finite(strength_max) ||
      strength_max <= 0
    )
  ) {
    stop(
      "`strength_max` must be `NULL` or a single finite positive numeric value.",
      call. = FALSE
    )
  }
  
  # -----------------------------------------------------------------------
  # Build matrix representation
  # -----------------------------------------------------------------------
  
  matrix_data <- build_network_change_matrix(
    x,
    row_guild = row_guild
  )
  
  cells <- matrix_data$cells
  
  # -----------------------------------------------------------------------
  # Strength encoding
  # -----------------------------------------------------------------------
  
  if (isTRUE(encode_strength)) {
    
    if (!isTRUE(x$comparison$weighted)) {
      stop(
        "`encode_strength = TRUE` is only meaningful for weighted networks.",
        call. = FALSE
      )
    }
    
    cells$Plot_strength <- ifelse(
      cells$Direction == "persistent",
      (
        cells$Strength_network_1 +
          cells$Strength_network_2
      ) / 2,
      ifelse(
        cells$Direction == "lost",
        cells$Strength_network_1,
        cells$Strength_network_2
      )
    )
    
    strength_scale_max <- if (is.null(strength_max)) {
      max(cells$Plot_strength, na.rm = TRUE)
    } else {
      strength_max
    }
    
    cells$Plot_alpha <-
      min_alpha +
      (1 - min_alpha) *
      pmin(
        cells$Plot_strength / strength_scale_max,
        1
      )
    
  } else {
    
    cells$Plot_alpha <- 1
  }
  
  # -----------------------------------------------------------------------
  # Base plot
  # -----------------------------------------------------------------------
  
  p <- ggplot2::ggplot(
    cells,
    ggplot2::aes(
      x = Column_index,
      y = Row_index
    )
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(
        fill = Direction,
        alpha = Plot_alpha
      ),
      width = 0.9,
      height = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        persistent = "#B3B3B3",
        lost = "#D55E00",
        gained = "#0072B2"
      ),
      breaks = c(
        "persistent",
        "lost",
        "gained"
      ),
      name = "Interaction change"
    ) +
    ggplot2::scale_alpha_identity()
  
  # -----------------------------------------------------------------------
  # Strength-direction symbols
  # -----------------------------------------------------------------------
  
  if (isTRUE(show_strength_symbols)) {
    
    symbol_cells <- cells[
      cells$Direction == "persistent" &
        cells$Strength_direction %in%
        c("strengthened", "weakened"),
      ,
      drop = FALSE
    ]
    
    if (nrow(symbol_cells) > 0L) {
      
      symbol_cells$Strength_symbol <-
        symbol_cells$Strength_direction
      
      p <- p +
        ggplot2::geom_point(
          data = symbol_cells,
          ggplot2::aes(
            x = Column_index,
            y = Row_index,
            shape = Strength_symbol
          ),
          inherit.aes = FALSE,
          size = 2.5,
          fill = "black"
        ) +
        ggplot2::scale_shape_manual(
          values = c(
            strengthened = 24,
            weakened = 25
          ),
          breaks = c(
            "strengthened",
            "weakened"
          ),
          name = "Strength change"
        )
    }
  }
  
  # -----------------------------------------------------------------------
  # Node-status boundaries
  # -----------------------------------------------------------------------
  
  if (length(matrix_data$row_boundaries) > 0L) {
    
    p <- p +
      ggplot2::geom_hline(
        yintercept = matrix_data$row_boundaries,
        linewidth = 0.4
      )
  }
  
  if (length(matrix_data$column_boundaries) > 0L) {
    
    p <- p +
      ggplot2::geom_vline(
        xintercept = matrix_data$column_boundaries,
        linewidth = 0.4
      )
  }
  
  # -----------------------------------------------------------------------
  # Guild boundaries
  # -----------------------------------------------------------------------
  
  if (length(matrix_data$row_guild_boundaries) > 0L) {
    
    p <- p +
      ggplot2::geom_hline(
        yintercept =
          matrix_data$row_guild_boundaries,
        linewidth = 0.8
      )
  }
  
  if (length(matrix_data$column_guild_boundaries) > 0L) {
    
    p <- p +
      ggplot2::geom_vline(
        xintercept =
          matrix_data$column_guild_boundaries,
        linewidth = 0.8
      )
  }
  
  # -----------------------------------------------------------------------
  # Axes
  # -----------------------------------------------------------------------
  
  if (isTRUE(show_node_labels)) {
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq_len(nrow(matrix_data$column_nodes)),
        labels = matrix_data$column_nodes$Node,
        limits = c(
          0.5,
          nrow(matrix_data$column_nodes) + 0.5
        ),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq_len(nrow(matrix_data$row_nodes)),
        labels = matrix_data$row_nodes$Node,
        limits = c(
          0.5,
          nrow(matrix_data$row_nodes) + 0.5
        ),
        expand = c(0, 0),
        trans = "reverse"
      )
    
  } else {
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = NULL,
        limits = c(
          0.5,
          nrow(matrix_data$column_nodes) + 0.5
        ),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = NULL,
        limits = c(
          0.5,
          nrow(matrix_data$row_nodes) + 0.5
        ),
        expand = c(0, 0),
        trans = "reverse"
      )
  }
  
  # -----------------------------------------------------------------------
  # Final formatting
  # -----------------------------------------------------------------------
  
  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ),
      axis.ticks = ggplot2::element_blank()
    )
  
  p
}