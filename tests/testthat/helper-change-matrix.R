# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

make_unipartite_matrix_changes <- function() {
  
  interactions <- data.frame(
    Site = rep("site_1", 4),
    Time = c("early", "early", "late", "late"),
    Origin_node = c("node_a", "node_a", "node_a", "node_a"),
    Destiny_node = c("node_b", "node_c", "node_b", "node_d"),
    Strength = c(2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  
  prepared <- prepare_interactions(
    interactions,
    directed = FALSE,
    multipartite = FALSE
  )
  
  quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
}


make_directed_multipartite_matrix_changes <- function() {
  
  interactions <- data.frame(
    Site = rep("site_1", 4),
    Time = c("early", "early", "late", "late"),
    Origin_node = c(
      "plant_1",
      "plant_2",
      "plant_1",
      "plant_1"
    ),
    Destiny_node = c(
      "pollinator_1",
      "pollinator_1",
      "pollinator_1",
      "pollinator_2"
    ),
    Strength = c(2, 3, 4, 5),
    Origin_guild = rep("plant", 4),
    Destiny_guild = rep("pollinator", 4),
    stringsAsFactors = FALSE
  )
  
  prepared <- prepare_interactions(
    interactions,
    directed = TRUE,
    multipartite = TRUE
  )
  
  quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
}


make_three_guild_matrix_changes <- function() {
  
  interactions <- data.frame(
    Site = rep("site_1", 4),
    Time = c("early", "early", "late", "late"),
    Origin_node = c(
      "node_a",
      "node_b",
      "node_a",
      "node_b"
    ),
    Destiny_node = c(
      "node_b",
      "node_c_old",
      "node_b",
      "node_c_new"
    ),
    Strength = c(2, 3, 4, 5),
    Origin_guild = c(
      "guild_a",
      "guild_b",
      "guild_a",
      "guild_b"
    ),
    Destiny_guild = c(
      "guild_b",
      "guild_c",
      "guild_b",
      "guild_c"
    ),
    stringsAsFactors = FALSE
  )
  
  prepared <- prepare_interactions(
    interactions,
    directed = FALSE,
    multipartite = TRUE
  )
  
  quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
}


make_undirected_two_guild_matrix_changes <- function() {
  
  interactions <- data.frame(
    Site = rep("site_1", 12),
    Time = c(
      rep("early", 6),
      rep("late", 6)
    ),
    
    Origin_node = c(
      # early
      "plant_1",
      "plant_2",
      "plant_lost",
      "plant_1",
      "plant_lost",
      "plant_1",
      
      # late
      "plant_1",
      "plant_2",
      "plant_gained",
      "plant_1",
      "plant_gained",
      "plant_1"
    ),
    
    Destiny_node = c(
      # early
      "bee_1",
      "bee_2",
      "bee_1",
      "bee_lost",
      "bee_lost",
      "bee_2",
      
      # late
      "bee_1",
      "bee_2",
      "bee_1",
      "bee_gained",
      "bee_gained",
      "bee_2"
    ),
    
    Strength = c(
      # early
      5, 3, 2, 4, 2, 2,
      
      # late
      7, 3, 3, 5, 2, 4
    ),
    
    Origin_guild = rep(
      "plant",
      12
    ),
    
    Destiny_guild = rep(
      "pollinator",
      12
    ),
    
    stringsAsFactors = FALSE
  )
  
  prepared <- prepare_interactions(
    interactions,
    directed = FALSE,
    multipartite = TRUE
  )
  
  quantify_network_changes(
    prepared,
    "site_1", "early",
    "site_1", "late"
  )
}


make_self_link_matrix_changes <- function() {
  
  data <- data.frame(
    Site = rep("site_1", 6),
    Time = c(
      "early", "early", "early",
      "late", "late", "late"
    ),
    Origin_node = c(
      "node_a", "node_a", "node_b",
      "node_a", "node_a", "node_b"
    ),
    Destiny_node = c(
      "node_a", "node_b", "node_c",
      "node_a", "node_b", "node_c"
    ),
    Strength = c(
      2, 3, 1,
      4, 5, 2
    ),
    stringsAsFactors = FALSE
  )
  
  prepared <- prepare_interactions(
    data,
    directed = FALSE,
    multipartite = FALSE,
    allow_self_links = TRUE
  )
  
  quantify_network_changes(
    prepared,
    site_1 = "site_1",
    time_1 = "early",
    site_2 = "site_1",
    time_2 = "late"
  )
}


expect_status_order <- function(nodes) {
  
  status_levels <- c(
    "permanent",
    "lost",
    "gained"
  )
  
  status_rank <- match(
    as.character(nodes$Node_status),
    status_levels
  )
  
  expect_true(
    all(diff(status_rank) >= 0)
  )
  
  for (status in status_levels) {
    
    selected <-
      as.character(nodes$Node_status) == status
    
    if (sum(selected) > 1L) {
      expect_identical(
        as.character(nodes$Node[selected]),
        sort(as.character(nodes$Node[selected]))
      )
    }
  }
}
