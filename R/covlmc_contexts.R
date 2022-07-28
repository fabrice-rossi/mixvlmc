#' Contexts number of a VLMC with covariates
#'
#' This function returns the total number of contexts of a VLMC with covariates.
#'
#' @param ct a fitted covlmc model.
#' @return the number of contexts present in the VLMC with covariates.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' m_nocovariate <- vlmc(dts)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 10)
#' # should be 4
#' context_number(m_cov)
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

#' Contexts of a VLMC with covariates
#'
#' This function returns the different contexts present in a VLMC with covariates.
#'
#'
#' @param ct a fitted covlmc model.
#' @param type result type (see details).
#' @param reverse logical (defaults to FALSE). See details.
#' @param ... additional arguments for the contexts function.
#'
#' @return the list of the contexts represented in this tree or a data.frame
#'   with more content.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(0, median(pc$active_power), max(pc$active_power))
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' m_nocovariate <- vlmc(dts)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' contexts(m_cov)
#' @export
contexts.covlmc <- function(ct, type = c("list", "data.frame"), reverse = FALSE, ...) {
  type <- match.arg(type)
  preres <- rec_covlmc_contexts(c(), ct, ct$vals)
  if (reverse) {
    preres <- lapply(preres, rev)
  }
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- ct$vals[0]
  }
  if (type == "list") {
    preres
  } else {
    data.frame(contexts = I(preres))
  }
}
