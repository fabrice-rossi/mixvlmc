##  context tree representation
new_ctx_tree <- function(vals, root = NULL) {
  if (is.null(root)) {
    root <- list(vals = vals)
  } else {
    assertthat::assert_that(is.list(root))
    root$vals <- vals
  }
  structure(root, class = "ctx_tree")
}

#' Test if the object is a context tree
#'
#' This function returns \code{TRUE} for context trees and \code{FALSE} for other objects.
#'
#' @param x an R object.
#' @return \code{TRUE} for context trees.
#' @export
is_ctx_tree <- function(x) {
  inherits(x, "ctx_tree")
}

assertthat::on_failure(is_ctx_tree) <- function(call, env) {
  paste0(deparse(call$x), " is not a ctx_tree")
}

#' @export
print.ctx_tree <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$vals, collapse = ", ")
  ), "\n")
  invisible(x)
}

#' State space of a context tree
#'
#' This function returns the state space of a context tree.
#'
#' @param ct a context tree.
#' @return the context space of the tree.
#'
#' @export
states <- function(ct) {
  assertthat::assert_that(is_ctx_tree(ct))
  ct$vals
}

rec_depth <- function(ct) {
  if (is.null(ct$children)) {
    0
  } else {
    1 + max(sapply(ct$children, rec_depth))
  }
}

#' Depth of a context tree
#'
#' This function return the depth of a context tree, i.e. the length of the
#' longest context represented in the tree.
#'
#' @param ct a context tree.
#' @return the depth of the tree.
#'
#' @export
depth <- function(ct) {
  assertthat::assert_that(is_ctx_tree(ct))
  rec_depth(ct)
}

nb_sub_tree <- function(ct) {
  if (is.null(ct) || is.null(ct$children) || length(ct$children) == 0) {
    0
  } else {
    sum(sapply(ct$children, length) > 0)
  }
}

rec_draw_depth_first <-
  function(prefix, ct, vals, terminals, probs) {
    ## check for pruned leaf
    if (length(ct) > 0) {
      for (v in seq_along(ct$children)) {
        rec_draw(
          paste0(prefix, vals[v]),
          ct$children[[v]],
          vals,
          terminals,
          probs
        )
      }
      if (!terminals || is.null(ct$children)) {
        cat(prefix)
        if (!is.null(ct$f_by)) {
          if (probs) {
            cat(paste0("\t", paste(
              ct$f_by / sum(ct$f_by),
              collapse = " "
            )), "\n")
          } else {
            cat(paste0("\t", paste(ct$f_by, collapse = " ")), "\n")
          }
        } else {
          cat("\n")
        }
      }
    }
  }

rec_draw <-
  function(prefix,
           rank,
           nst,
           ct,
           vals,
           terminals,
           probs) {
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
      if (!is.null(ct$f_by)) {
        if (probs) {
          cat(paste0(" (", paste(ct$f_by / sum(ct$f_by), collapse = ", "), ")"), "\n")
        } else {
          cat(paste0(" (", paste(ct$f_by, collapse = ", "), ")"), "\n")
        }
      } else {
        cat("\n")
      }
      # then go down the tree
      nst <- nb_sub_tree(ct)
      if (nst > 1) {
        prefix <- paste0(prefix, "| ")
      } else {
        prefix <- paste0(prefix, "  ")
      }
      for (v in seq_along(ct$children)) {
        rec_draw(prefix, v, nst, ct$children[[v]], vals, terminals, probs)
      }
    }
  }


#' Text based representation of a context tree
#'
#' This function 'draws' a context tree as a text.
#'
#' @param ct a context tree.
#' @param terminals not used
#' @param probs logical; if \code{TRUE}, display the next state distribution rather than the data counts
#' @return the context tree (invisibly)
#'
#' @export
draw <- function(ct,
                 terminals = FALSE,
                 probs = FALSE) {
  assertthat::assert_that(is_ctx_tree(ct))
  rec_draw("", 0, length(ct$vals), ct, ct$vals, terminals, probs)
  invisible(ct)
}
