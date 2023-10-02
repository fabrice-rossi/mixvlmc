#' Find the parent of a node in a context tree
#'
#' This function returns the parent node of the node represented by the
#' `node` parameter. The result is `NULL` if `node` is the root node of
#' its context tree (representing the empty sequence).
#'
#' Each node of a context tree represents a sequence. When [find_sequence()] is
#' called with success, the returned object represents the corresponding node in
#' the context tree. Unless the original sequence is empty, this node has a
#' parent node which is returned as a `ctx_node` object by the present function.
#' Another interpretation is that the function returns the `node` object
#' associated to the sequence obtained by removing the oldest value from the
#' original sequence.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @returns a `ctx_node` object if `node` does correspond to the empty
#'   sequence or `NULL` when this is not the case
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' ctx_00 <- find_sequence(dts_ctree, c(0, 0))
#' ## the parent sequence/node corresponds to the 0 context
#' parent(ctx_00)
#' identical(parent(ctx_00), find_sequence(dts_ctree, c(0)))
#' @export
parent <- function(node) {
  assertthat::assert_that(is_ctx_node(node))
  if (length(node$sequence) >= 1) {
    find_sequence(node$tree, node$sequence[-length(node$sequence)])
  } else {
    NULL
  }
}

#' Find the children nodes of a node in a context tree
#'
#' This function returns a list (possibly empty) of `ctx_node` objects. Each
#' object represents one of the children of the node represented by the
#' `node` parameter.
#'
#' Each node of a context tree represents a sequence. When [find_sequence()] is
#' called with success, the returned object represents the corresponding
#' node in the context tree. If this node has no child, the present
#' function returns an empty list. When the node has at least one child, the
#' function returns a list with one value for each element in the state space
#' (see [states()]). The value is `NULL` if the corresponding child is empty,
#' while it is a `ctx_node` object when the child is present. Each `ctx_node`
#' object is associated to the sequence obtained by adding to the past of
#' the sequence represented by `node` an observation of the associated state.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @returns a list of `ctx_node` objects, see details.
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' ctx_00 <- find_sequence(dts_ctree, c(0, 0))
#' ## this context can only be extended in the past by 1:
#' children(ctx_00)
#' ctx_01 <- find_sequence(dts_ctree, c(0, 1))
#' ## this context can be extended by both states
#' children(ctx_01)
#' @export
children <- function(node) {
  assertthat::assert_that(is_ctx_node(node))
  if (is.null(node$node[["children"]])) {
    list()
  } else {
    res <- vector(mode = "list", length = length(node$node[["children"]]))
    for (k in seq_along(res)) {
      if (length(node$node$children[[k]]) > 0) {
        the_ctx <- c(node$sequence, node$tree$vals[k])
        res[[k]] <- new_ctx_node(the_ctx, node$tree, node$node$children[[k]])
      }
    }
    names(res) <- as.character(node$tree$vals)
    res
  }
}
