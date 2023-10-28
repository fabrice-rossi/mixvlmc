ctx_recode <- function(ctxs, reverse, vals) {
  if (reverse) {
    lapply(ctxs, function(x) vals[x + 1])
  } else {
    lapply(ctxs, function(x) vals[rev(x) + 1])
  }
}

#' @export
#' @rdname contexts.ctx_tree
contexts.ctx_tree_cpp <- function(ct, sequence = FALSE,
                                  reverse = FALSE, frequency = NULL,
                                  positions = FALSE, ...) {
  restore_ctx_tree_cpp(ct)
  if (!is.null(frequency)) {
    assertthat::assert_that(frequency %in% c("total", "detailed"))
  }
  wants_df <- !is.null(frequency) || positions
  if (missing(sequence)) {
    sequence <- wants_df
  } else {
    if (!sequence && wants_df) {
      stop("sequence = 'FALSE' is incompatible with with the other requested values")
    }
  }
  if (!sequence) {
    ## contexts are returned in reverse order
    pre_res <- ct$root$raw_contexts()
    res <- vector(mode = "list", length = length(pre_res$ptrs))
    for (k in seq_along(res)) {
      ctx <- ct$vals[pre_res$ctxs[[k]] + 1]
      res[[k]] <- new_ctx_node_cpp(ctx, ct, pre_res$ptrs[[k]], reverse)
    }
    new_context_list(res)
  } else {
    if (is.null(frequency) && !positions) {
      pre_res <- ct$root$contexts(1, -1)
      pre_res <- ctx_recode(pre_res, reverse, ct$vals)
      data.frame(context = I(pre_res))
    } else {
      with_freq <- FALSE
      pre_res <- ct$root$full_contexts(1, -1, positions, FALSE, FALSE)
      res <- data.frame(context = I(ctx_recode(pre_res$context, reverse, ct$vals)))
      if (!is.null(frequency)) {
        if (frequency == "detailed") {
          res <- cbind(res, pre_res$counts)
          names(res)[3:(2 + length(ct$vals))] <- ct$vals
        } else {
          res$freq <- pre_res$counts$freq
        }
      }
      if (positions) {
        res$positions <- I(pre_res$positions)
      }
      res
    }
  }
}
