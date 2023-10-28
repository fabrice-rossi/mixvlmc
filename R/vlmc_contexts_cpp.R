#' @export
#' @rdname contexts.vlmc
contexts.vlmc_cpp <- function(ct, sequence = FALSE, reverse = FALSE, frequency = NULL,
                              positions = FALSE, counts = c("desc", "local"), cutoff = NULL, metrics = FALSE, ...) {
  restore_model(ct)
  counts <- match.arg(counts)
  if (!is.null(frequency)) {
    assertthat::assert_that(frequency %in% c("total", "detailed"))
  }
  if (!is.null(cutoff)) {
    assertthat::assert_that(cutoff %in% c("quantile", "native"))
  }
  wants_df <- !is.null(frequency) || positions || !is.null(cutoff) || metrics
  if (missing(sequence)) {
    sequence <- wants_df
  } else {
    if (!sequence && wants_df) {
      stop("sequence = 'FALSE' is incompatible with the other requested values")
    }
  }
  if (!sequence) {
    pre_res <- ct$root$raw_contexts()
    res <- vector(mode = "list", length = length(pre_res$ptrs))
    for (k in seq_along(res)) {
      ctx <- ct$vals[pre_res$ctxs[[k]] + 1]
      res[[k]] <- new_ctx_node_cpp(ctx, ct, pre_res$ptrs[[k]], reverse)
    }
    new_context_list(res)
  } else {
    with_local <- metrics || counts == "local"
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
    }
    pre_res <- ct$root$full_contexts(1, -1, positions, !is.null(cutoff), with_local)
    res <- data.frame(context = I(ctx_recode(pre_res$context, reverse, ct$vals)))
    if (!is.null(frequency)) {
      if (frequency == "detailed") {
        if (counts == "local") {
          res <- cbind(res, pre_res$local_counts)
        } else {
          res <- cbind(res, pre_res$counts)
        }
        names(res)[3:(2 + length(ct$vals))] <- ct$vals
      } else {
        if (counts == "local") {
          res$freq <- pre_res$local_counts$freq
        } else {
          res$freq <- pre_res$counts$freq
        }
      }
    }
    if (positions) {
      res$positions <- I(pre_res$positions)
    }
    if (!is.null(cutoff)) {
      if ((cutoff == "quantile")) {
        res$cutoff <- to_quantile(pre_res$cutoff, length(ct$vals))
      } else {
        res$cutoff <- pre_res$cutoff
      }
    }
    if (metrics) {
      the_metrics <- NULL
      for (k in 1:nrow(res)) {
        fake_data <- generate_fake_data(
          pre_res$local_counts[k, 1],
          pre_res$local_counts[k, -1, drop = FALSE],
          pre_res$counts[k, -1, drop = FALSE] / pre_res$counts[k, 1],
          ct$vals
        )
        local_m <- main_metrics(fake_data$response, fake_data$predictor)
        local_m$roc <- NULL
        local_m$conf_mat <- NULL
        if (is.null(the_metrics)) {
          the_metrics <- local_m
          for (p in 1:length(the_metrics)) {
            the_metrics[[p]] <- rep(NA, nrow(res))
            the_metrics[[p]][1] <- local_m[[p]][1]
          }
        } else {
          for (p in 1:length(the_metrics)) {
            the_metrics[[p]][k] <- local_m[[p]][1]
          }
        }
      }
      res <- cbind(res, as.data.frame(the_metrics))
    }
    res
  }
}
