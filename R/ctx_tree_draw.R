rec_draw <- function(prefix, rank, nst, ct, vals, node2txt, ...) {
  ## check for pruned leaf
  if (length(ct) > 0) {
    # first print the current content
    if (rank > 0) {
      if (nst > 1 & rank == 1) {
        local_prefix <- "+ "
      } else {
        local_prefix <- "' "
      }
      local_prefix <- paste0(local_prefix, vals[rank])
    } else {
      local_prefix <- ""
    }
    cat(paste0(prefix, local_prefix))
    if (!is.null(node2txt)) {
      cat(" (", node2txt(ct, ...), ")")
    }
    cat("\n")
    # then go down the tree
    nst <- nb_sub_tree(ct)
    if (nst > 1) {
      prefix <- paste0(prefix, "| ")
    } else {
      prefix <- paste0(prefix, "  ")
    }
    for (v in seq_along(ct$children)) {
      rec_draw(prefix, v, nst, ct$children[[v]], vals, node2txt, ...)
    }
  }
}

#' Text based representation of a context tree
#'
#' This function 'draws' a context tree as a text.
#'
#' @param ct a context tree.
#' @param node2txt an optional function called on each node to render it to a text representation.
#' @param ... additional arguments for node2txt.
#' @return the context tree (invisibly).
#'
#' @export
draw <- function(ct, node2txt = NULL, ...) {
  UseMethod("draw")
  invisible(ct)
}

#' @export
draw.ctx_tree <- function(ct, node2txt = NULL, ...) {
  rec_draw("", 0, length(ct$vals), ct, ct$vals, node2txt, ...)
  invisible(ct)
}
