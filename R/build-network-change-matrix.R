# =============================================================================
# Construction of matrices for visualizing network changes
# =============================================================================

#' Build a matrix representation of network changes
#'
#' Constructs the internal matrix representation used by
#' [plot_network_changes_matrix()] to visualize changes between two
#' interaction networks.
#'
#' The representation is built directly from a `network_changes` object and
#' therefore uses the node- and link-level classifications already calculated
#' by [quantify_network_changes()]. No network-change categories are
#' recalculated internally.
#'
#' The matrix layout is selected according to network topology and direction:
#'
#' * undirected multipartite networks with exactly two guilds are represented
#'   as rectangular interaction matrices;
#' * unipartite networks are represented as square adjacency matrices;
#' * directed multipartite networks and multipartite networks with more than
#'   two guilds are represented as square adjacency matrices.
#'
#' For rectangular interaction matrices, one guild is assigned to rows and
#' the other to columns. For adjacency matrices, rows and columns contain the
#' same ordered set of nodes.
#'
#' Nodes are ordered using `order_change_matrix_nodes()`: permanent nodes
#' first, followed by lost and gained nodes, with alphabetical ordering within
#' each status group. For multipartite adjacency matrices, guilds are first
#' arranged as contiguous alphabetical blocks and the same node-status
#' ordering is then applied within each guild.
#'
#' For undirected adjacency matrices, each classified interaction is
#' represented symmetrically in both matrix directions. Directed adjacency
#' matrices retain the original `Origin_node` to `Destiny_node` orientation.
#'
#' @param x A `network_changes` object created by
#'   [quantify_network_changes()].
#' @param row_guild Optional character value specifying the guild to place on
#'   the rows of an undirected two-guild multipartite interaction matrix.
#'   If `NULL`, guild names are ordered alphabetically and the first guild is
#'   assigned to rows. The remaining guild is assigned to columns.
#'   Ignored for adjacency matrices.
#'
#' @return A named list containing:
#'
#' * `layout`: matrix representation, either `"interaction"` or
#'   `"adjacency"`;
#' * `directed`: logical value indicating whether the original network is
#'   directed;
#' * `row_guild`: guild assigned to rows for interaction matrices, otherwise
#'   `NULL`;
#' * `column_guild`: guild assigned to columns for interaction matrices,
#'   otherwise `NULL`;
#' * `row_nodes`: ordered node information for matrix rows;
#' * `column_nodes`: ordered node information for matrix columns;
#' * `cells`: link-level classification data together with `Row_node`,
#'   `Column_node`, `Row_index`, and `Column_index`;
#' * `impossible_cells`: matrix positions corresponding to node pairs that
#'   cannot represent a valid interaction in either compared network because
#'   the nodes do not coexist, because the interaction would violate
#'   multipartite guild structure, or because it represents a disallowed
#'   self-link;
#' * `row_boundaries`: positions separating node-status groups along rows;
#' * `column_boundaries`: positions separating node-status groups along
#'   columns;
#' * `row_guild_boundaries`: positions separating guild blocks along rows
#'   when applicable;
#' * `column_guild_boundaries`: positions separating guild blocks along
#'   columns when applicable.
#'
#' Matrix cells that cannot represent a valid interaction are identified
#' separately in `impossible_cells`. These include combinations between a
#' lost and a gained node, since those nodes do not coexist in either network.
#' For multipartite adjacency matrices, within-guild combinations are also
#' considered impossible. For unipartite adjacency matrices, diagonal cells
#' are considered impossible when self-links are not allowed.
#'
#' Empty matrix cells not included in `impossible_cells` represent
#' interactions that are structurally possible but were not observed in
#' either network.
#'
#' Boundary positions are expressed halfway between adjacent matrix cells
#' (for example, `3.5` separates cells 3 and 4) so that they can be used
#' directly by plotting functions.
#'
#' @keywords internal
build_network_change_matrix <- function(
    x,
    row_guild = NULL
) {
  
  validate_network_changes(x)
  
  nodes <- x$nodes
  links <- x$links
  
  topology <- x$comparison$topology
  directed <- x$comparison$directed
  allow_self_links <- x$comparison$allow_self_links
  
  # -----------------------------------------------------------------------
  # Determine matrix layout
  # -----------------------------------------------------------------------
  
  has_guild <- "Guild" %in% names(nodes)
  
  guilds <- if (has_guild) {
    sort(unique(as.character(nodes$Guild)))
  } else {
    character(0)
  }
  
  guilds <- guilds[!is.na(guilds) & nzchar(guilds)]
  
  if (topology == "multipartite" && length(guilds) < 2L) {
    stop(
      "Multipartite network changes require at least two guilds in `nodes`.",
      call. = FALSE
    )
  }
  
  use_interaction_matrix <-
    topology == "multipartite" &&
    !isTRUE(directed) &&
    length(guilds) == 2L
  
  layout <- if (use_interaction_matrix) {
    "interaction"
  } else {
    "adjacency"
  }
  
  # -----------------------------------------------------------------------
  # Helper: boundaries between node-status groups
  # -----------------------------------------------------------------------
  
  status_boundaries <- function(node_table) {
    
    if (nrow(node_table) <= 1L) {
      return(numeric(0))
    }
    
    status <- as.character(node_table$Node_status)
    
    which(status[-1L] != status[-length(status)]) + 0.5
  }
  
  # -----------------------------------------------------------------------
  # Helper: boundaries between guild groups
  # -----------------------------------------------------------------------
  
  guild_boundaries <- function(node_table) {
    
    if (
      !"Guild" %in% names(node_table) ||
      nrow(node_table) <= 1L
    ) {
      return(numeric(0))
    }
    
    guild <- as.character(node_table$Guild)
    
    which(guild[-1L] != guild[-length(guild)]) + 0.5
  }
  
  # -----------------------------------------------------------------------
  # Helper: structurally impossible matrix cells
  # -----------------------------------------------------------------------
  
  impossible_matrix_cells <- function(
    row_nodes,
    column_nodes,
    layout,
    topology,
    allow_self_links
  ) {
    
    grid <- expand.grid(
      Row_index = seq_len(nrow(row_nodes)),
      Column_index = seq_len(nrow(column_nodes)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    
    grid$Row_node <- as.character(
      row_nodes$Node[grid$Row_index]
    )
    
    grid$Column_node <- as.character(
      column_nodes$Node[grid$Column_index]
    )
    
    grid$Row_status <- as.character(
      row_nodes$Node_status[grid$Row_index]
    )
    
    grid$Column_status <- as.character(
      column_nodes$Node_status[grid$Column_index]
    )
    
    # Lost and gained nodes never coexist in either network.
    temporal_impossible <-
      (
        grid$Row_status == "lost" &
          grid$Column_status == "gained"
      ) |
      (
        grid$Row_status == "gained" &
          grid$Column_status == "lost"
      )
    
    topology_impossible <- rep(
      FALSE,
      nrow(grid)
    )
    
    # Within-guild links are impossible in multipartite adjacency matrices.
    if (
      layout == "adjacency" &&
      topology == "multipartite"
    ) {
      
      row_guild <- as.character(
        row_nodes$Guild[grid$Row_index]
      )
      
      column_guild <- as.character(
        column_nodes$Guild[grid$Column_index]
      )
      
      topology_impossible <-
        row_guild == column_guild
    }
    
    self_link_impossible <- rep(
      FALSE,
      nrow(grid)
    )
    
    # Diagonal cells are impossible when self-links are not allowed.
    if (
      layout == "adjacency" &&
      topology == "unipartite" &&
      !isTRUE(allow_self_links)
    ) {
      
      self_link_impossible <-
        grid$Row_node == grid$Column_node
    }
    
    impossible <-
      temporal_impossible |
      topology_impossible |
      self_link_impossible
    
    result <- grid[
      impossible,
      c(
        "Row_index",
        "Column_index",
        "Row_node",
        "Column_node"
      ),
      drop = FALSE
    ]
    
    rownames(result) <- NULL
    
    result
  }
  
  # -----------------------------------------------------------------------
  # Rectangular interaction matrix
  # -----------------------------------------------------------------------
  
  if (layout == "interaction") {
    
    if (!is.null(row_guild)) {
      
      if (
        length(row_guild) != 1L ||
        is.na(row_guild) ||
        !is.character(row_guild) ||
        !row_guild %in% guilds
      ) {
        stop(
          "`row_guild` must identify one of the two guilds in the network.",
          call. = FALSE
        )
      }
      
    } else {
      
      row_guild <- guilds[1L]
    }
    
    column_guild <- setdiff(
      guilds,
      row_guild
    )
    
    row_nodes <- nodes[
      as.character(nodes$Guild) == row_guild,
      ,
      drop = FALSE
    ]
    
    column_nodes <- nodes[
      as.character(nodes$Guild) == column_guild,
      ,
      drop = FALSE
    ]
    
    row_nodes <- order_change_matrix_nodes(row_nodes)
    column_nodes <- order_change_matrix_nodes(column_nodes)
    
    row_lookup <- stats::setNames(
      seq_len(nrow(row_nodes)),
      as.character(row_nodes$Node)
    )
    
    column_lookup <- stats::setNames(
      seq_len(nrow(column_nodes)),
      as.character(column_nodes$Node)
    )
    
    node_guild_lookup <- stats::setNames(
      as.character(nodes$Guild),
      as.character(nodes$Node)
    )
    
    origin_guild <- unname(
      node_guild_lookup[as.character(links$Origin_node)]
    )
    
    destiny_guild <- unname(
      node_guild_lookup[as.character(links$Destiny_node)]
    )
    
    origin_is_row <-
      origin_guild == row_guild &
      destiny_guild == column_guild
    
    destiny_is_row <-
      destiny_guild == row_guild &
      origin_guild == column_guild
    
    if (!all(origin_is_row | destiny_is_row)) {
      stop(
        "Some links are inconsistent with the two-guild multipartite ",
        "structure.",
        call. = FALSE
      )
    }
    
    cells <- links
    
    cells$Row_node <- ifelse(
      origin_is_row,
      as.character(links$Origin_node),
      as.character(links$Destiny_node)
    )
    
    cells$Column_node <- ifelse(
      origin_is_row,
      as.character(links$Destiny_node),
      as.character(links$Origin_node)
    )
    
    cells$Row_index <- unname(
      row_lookup[cells$Row_node]
    )
    
    cells$Column_index <- unname(
      column_lookup[cells$Column_node]
    )
    
    cells <- cells[
      order(
        cells$Row_index,
        cells$Column_index
      ),
      ,
      drop = FALSE
    ]
    
    rownames(cells) <- NULL
    
    impossible_cells <- impossible_matrix_cells(
      row_nodes = row_nodes,
      column_nodes = column_nodes,
      layout = layout,
      topology = topology,
      allow_self_links = allow_self_links
    )
    
    return(list(
      layout = layout,
      directed = directed,
      row_guild = row_guild,
      column_guild = column_guild,
      row_nodes = row_nodes,
      column_nodes = column_nodes,
      cells = cells,
      impossible_cells = impossible_cells,
      row_boundaries = status_boundaries(row_nodes),
      column_boundaries = status_boundaries(column_nodes),
      row_guild_boundaries = numeric(0),
      column_guild_boundaries = numeric(0)
    ))
  }
  
  # -----------------------------------------------------------------------
  # Square adjacency matrix
  # -----------------------------------------------------------------------
  
  if (topology == "multipartite") {
    
    # Keep guilds as contiguous blocks. Within each guild, nodes are ordered
    # by status and then alphabetically.
    ordered_by_guild <- lapply(
      guilds,
      function(guild) {
        
        guild_nodes <- nodes[
          as.character(nodes$Guild) == guild,
          ,
          drop = FALSE
        ]
        
        order_change_matrix_nodes(guild_nodes)
      }
    )
    
    ordered_nodes <- do.call(
      rbind,
      ordered_by_guild
    )
    
    rownames(ordered_nodes) <- NULL
    
  } else {
    
    # Guild information, if present, does not define the ordering of
    # unipartite networks.
    ordered_nodes <- order_change_matrix_nodes(nodes)
  }
  
  row_nodes <- ordered_nodes
  column_nodes <- ordered_nodes
  
  node_lookup <- stats::setNames(
    seq_len(nrow(ordered_nodes)),
    as.character(ordered_nodes$Node)
  )
  
  cells <- links
  
  cells$Row_node <- as.character(
    links$Origin_node
  )
  
  cells$Column_node <- as.character(
    links$Destiny_node
  )
  
  cells$Row_index <- unname(
    node_lookup[cells$Row_node]
  )
  
  cells$Column_index <- unname(
    node_lookup[cells$Column_node]
  )
  
  # -----------------------------------------------------------------------
  # Mirror undirected adjacency matrices
  # -----------------------------------------------------------------------
  
  if (!isTRUE(directed)) {
    
    off_diagonal <-
      cells$Row_node != cells$Column_node
    
    mirrored <- cells[
      off_diagonal,
      ,
      drop = FALSE
    ]
    
    mirrored$Row_node <-
      cells$Column_node[off_diagonal]
    
    mirrored$Column_node <-
      cells$Row_node[off_diagonal]
    
    mirrored$Row_index <-
      cells$Column_index[off_diagonal]
    
    mirrored$Column_index <-
      cells$Row_index[off_diagonal]
    
    cells <- rbind(
      cells,
      mirrored
    )
  }
  
  cells <- cells[
    order(
      cells$Row_index,
      cells$Column_index
    ),
    ,
    drop = FALSE
  ]
  
  rownames(cells) <- NULL
  
  # -----------------------------------------------------------------------
  # Return matrix representation
  # -----------------------------------------------------------------------
  
  impossible_cells <- impossible_matrix_cells(
    row_nodes = row_nodes,
    column_nodes = column_nodes,
    layout = layout,
    topology = topology,
    allow_self_links = allow_self_links
  )
  
  list(
    layout = layout,
    directed = directed,
    row_guild = NULL,
    column_guild = NULL,
    row_nodes = row_nodes,
    column_nodes = column_nodes,
    cells = cells,
    impossible_cells = impossible_cells,
    row_boundaries = status_boundaries(row_nodes),
    column_boundaries = status_boundaries(column_nodes),
    row_guild_boundaries = if (topology == "multipartite") {
      guild_boundaries(row_nodes)
    } else {
      numeric(0)
    },
    column_guild_boundaries = if (topology == "multipartite") {
      guild_boundaries(column_nodes)
    } else {
      numeric(0)
    }
  )
}