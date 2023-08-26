#' @export
contexts.vlmc_cpp <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, frequency = NULL,
                              positions = FALSE, counts = c("desc", "local"), cutoff = NULL, metrics = FALSE, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  type <- match.arg(type)
  counts <- match.arg(counts)
  if (is.null(cutoff) && counts == "desc" && !metrics) {
    NextMethod()
  } else {
    assertthat::assert_that(type %in% c("auto", "data.frame"))
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
    }
    if (!is.null(cutoff)) {
      assertthat::assert_that(cutoff %in% c("quantile", "native"))
    }
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
