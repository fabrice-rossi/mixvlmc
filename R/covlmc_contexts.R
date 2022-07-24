#' Contexts number of a VLMC with covariates
#'
#' This function returns the total number of contexts of a VLMC with covariates.
#'
#' @param vlmc a fitted covlmc model
#' @return the number of contexts present in the VLMC with covariates.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' m_nocovariate <- vlmc(dts)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' # should be 5
#' context_number(m_cov)
#' @export
context_number.covlmc <- function(vlmc) {
  if (!is.null(vlmc$nb_ctx)) {
    vlmc$nb_ctx
  } else {
    rec_context_number(vlmc, count_covlmc_local_context)
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
