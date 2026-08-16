# =============================================================================
# Tests for order_change_matrix_nodes()
# =============================================================================

test_that("nodes are ordered by status and alphabetically within status", {
  nodes <- data.frame(
    Node = c(
      "bee_c",
      "plant_b",
      "bee_a",
      "plant_a",
      "bee_b",
      "plant_c"
    ),
    Node_status = c(
      "gained",
      "permanent",
      "lost",
      "permanent",
      "lost",
      "gained"
    ),
    stringsAsFactors = FALSE
  )
  
  result <- order_change_matrix_nodes(nodes)
  
  expect_identical(
    result$Node_status,
    c(
      "permanent",
      "permanent",
      "lost",
      "lost",
      "gained",
      "gained"
    )
  )
  
  expect_identical(
    result$Node,
    c(
      "plant_a",
      "plant_b",
      "bee_a",
      "bee_b",
      "bee_c",
      "plant_c"
    )
  )
})


test_that("all rows and columns are preserved", {
  nodes <- data.frame(
    Node = c("node_c", "node_a", "node_b"),
    Node_status = c("gained", "permanent", "lost"),
    Guild = c("pollinator", "plant", "pollinator"),
    Present_network_1 = c(FALSE, TRUE, TRUE),
    Present_network_2 = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  result <- order_change_matrix_nodes(nodes)
  
  expect_identical(names(result), names(nodes))
  expect_equal(nrow(result), nrow(nodes))
  
  expect_setequal(
    result$Node,
    nodes$Node
  )
})


test_that("ordering works when some node-status categories are absent", {
  nodes <- data.frame(
    Node = c(
      "node_d",
      "node_b",
      "node_c",
      "node_a"
    ),
    Node_status = c(
      "gained",
      "permanent",
      "gained",
      "permanent"
    ),
    stringsAsFactors = FALSE
  )
  
  result <- order_change_matrix_nodes(nodes)
  
  expect_identical(
    result$Node_status,
    c(
      "permanent",
      "permanent",
      "gained",
      "gained"
    )
  )
  
  expect_identical(
    result$Node,
    c(
      "node_a",
      "node_b",
      "node_c",
      "node_d"
    )
  )
})


test_that("ordering works with a single node-status category", {
  nodes <- data.frame(
    Node = c("node_c", "node_a", "node_b"),
    Node_status = rep("permanent", 3),
    stringsAsFactors = FALSE
  )
  
  result <- order_change_matrix_nodes(nodes)
  
  expect_identical(
    result$Node,
    c("node_a", "node_b", "node_c")
  )
  
  expect_true(all(
    result$Node_status == "permanent"
  ))
})


test_that("row names are reset after ordering", {
  nodes <- data.frame(
    Node = c("node_c", "node_a", "node_b"),
    Node_status = c("gained", "permanent", "lost"),
    stringsAsFactors = FALSE
  )
  
  rownames(nodes) <- c("row_c", "row_a", "row_b")
  
  result <- order_change_matrix_nodes(nodes)
  
  expect_identical(
    rownames(result),
    as.character(seq_len(nrow(result)))
  )
})


test_that("missing required columns are rejected", {
  missing_node <- data.frame(
    Node_status = c("permanent", "lost"),
    stringsAsFactors = FALSE
  )
  
  missing_status <- data.frame(
    Node = c("node_a", "node_b"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    order_change_matrix_nodes(missing_node),
    "`nodes` is missing required columns: Node"
  )
  
  expect_error(
    order_change_matrix_nodes(missing_status),
    "`nodes` is missing required columns: Node_status"
  )
})


test_that("invalid node statuses are rejected", {
  nodes <- data.frame(
    Node = c("node_a", "node_b"),
    Node_status = c("permanent", "transient"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    order_change_matrix_nodes(nodes),
    "`Node_status` contains invalid values: transient"
  )
})