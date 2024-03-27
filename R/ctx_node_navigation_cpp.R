#' @export
#' @rdname parent
#' @examples
#' ## C++ backend
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
#' ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
#' ## the parent sequence/node corresponds to the 0 context
#' parent(ctx_00)
#' identical(parent(ctx_00), find_sequence(rdts_ctree, c(0)))
parent.ctx_node_cpp <- function(node) {
  restore_ctx_node_cpp(node)
  if (length(node$sequence) >= 1) {
    parent_ptr <- node$tree$root$node_parent(node$node_env$node, length(node$sequence))
    new_ctx_node_cpp(node$sequence[-length(node$sequence)], node$tree, parent_ptr, node$rev)
  } else {
    NULL
  }
}

#' @export
#' @rdname children
#' @examples
#' ## C++ backend
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
#' ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
#' ## this context can only be extended in the past by 1:
#' children(ctx_00)
#' ctx_10 <- find_sequence(rdts_ctree, c(1, 0))
#' ## this context can be extended by both states
#' children(ctx_10)
children.ctx_node_cpp <- function(node) {
  restore_ctx_node_cpp(node)
  raw_result <- node$tree$root$node_children(node$node_env$node, length(node$sequence))
  one_child <- FALSE
  for (k in seq_along(raw_result)) {
    if (!is.null(raw_result[[k]])) {
      raw_result[[k]] <- new_ctx_node_cpp(
        c(node$sequence, node$tree$vals[k]),
        node$tree,
        raw_result[[k]],
        node$rev
      )
      one_child <- TRUE
    }
  }
  if (one_child) {
    names(raw_result) <- as.character(node$tree$vals)
    raw_result
  } else {
    list()
  }
}
