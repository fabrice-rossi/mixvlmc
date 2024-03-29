#' Trim a context tree
#'
#' This function returns a trimmed context tree from which match positions
#' have been removed.
#'
#' @param ct a context tree.
#' @param ... additional arguments for the trim function.
#'
#' @returns a trimmed context tree.
#' @export
#'
#' @examples
#' ## context tree trimming
#' rdts <- sample(as.factor(c("A", "B", "C")), 1000, replace = TRUE)
#' rdts_tree <- ctx_tree(rdts, max_depth = 10, min_size = 5, keep_position = TRUE)
#' print(object.size(rdts_tree))
#' rdts_tree <- trim(rdts_tree)
#' print(object.size(rdts_tree))
trim <- function(ct, ...) {
  UseMethod("trim")
}

rec_trim_ctx_tree <- function(ct) {
  if (is.null(ct$children)) {
    ct$match <- NULL
    ct
  } else {
    for (k in seq_along(ct$children)) {
      if (length(ct$children[[k]]) > 0) {
        ct$children[[k]] <- rec_trim_ctx_tree(ct$children[[k]])
      }
    }
    ct$match <- NULL
    ct
  }
}

#' @export
trim.ctx_tree <- function(ct, ...) {
  rec_trim_ctx_tree(ct)
}
