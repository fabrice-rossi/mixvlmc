#' @export
context_number.covlmc <- function(ct) {
  if (!is.null(ct$nb_ctx)) {
    ct$nb_ctx
  } else {
    rec_context_number(ct, count_covlmc_local_context)
  }
}

rec_covlmc_contexts <- function(path, ct, vals) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is model, then this is a context
    if (is.null(ct$model)) {
      NULL
    } else {
      list(path)
    }
  } else {
    all_ctx <- list()
    for (v in seq_along(ct$children)) {
      sub_ctx <- rec_covlmc_contexts(c(path, vals[v]), ct$children[[v]], vals)
      if (!is.null(sub_ctx)) {
        all_ctx <- c(all_ctx, sub_ctx)
      }
    }
    ## we may have merged model which corresponds to multiple contexts at once
    if (!is.null(ct$merged_model)) {
      for (v in ct$merged) {
        all_ctx <- c(all_ctx, list(c(path, vals[v])))
      }
    }
    all_ctx
  }
}

#' @export
contexts.covlmc <- function(ct) {
  preres <- rec_covlmc_contexts(c(), ct, ct$vals)
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- list()
  }
  preres
}
