local_loglikelihood_vlmc <- function(model_counts, data_counts = NULL) {
  if (is.null(data_counts)) {
    data_counts <- model_counts
  }
  sc <- sum(model_counts)
  if (sc > 0) {
    probs <- model_counts / sc
    sum(data_counts * ifelse(probs > 0, log(probs), 0))
  } else {
    0
  }
}

rec_loglikelihood_vlmc <- function(tree) {
  if (is.null(tree$f_by)) {
    # place holder list
    NA
  } else if (is.null(tree$children)) {
    ## simple leaf case
    local_loglikelihood_vlmc(tree$f_by, tree$data_f_by)
  } else {
    ## recursive case
    all_ll <- sapply(tree$children, rec_loglikelihood_vlmc)
    sub_ll <- sum(all_ll, na.rm = TRUE)
    ## is the node a valid context
    if (anyNA(all_ll)) {
      ## let us add the local contribution
      sub_trees <- sapply(tree$children, function(x) !is.null(x$f_by))
      sub_counts <- rowSums(sapply(tree$children[sub_trees], function(x) x$f_by))
      loc_counts <- tree$f_by - sub_counts
      if (is.null(tree$data_f_by)) {
        sub_ll <- sub_ll + local_loglikelihood_vlmc(loc_counts)
      } else {
        data_sub_counts <- rowSums(sapply(tree$children[sub_trees], function(x) x$data_f_by))
        data_loc_counts <- tree$data_f_by - data_sub_counts
        sub_ll <- sub_ll + local_loglikelihood_vlmc(loc_counts, data_loc_counts)
      }
    }
    sub_ll
  }
}

#' @export
logLik.vlmc <- function(object, ...) {
  ll <- rec_loglikelihood_vlmc(object)
  attr(ll, "df") <- object$nb_ctx * (length(object$vals) - 1)
  attr(ll, "nobs") <- sum(object$f_by)
  class(ll) <- "logLik"
  ll
}

#' Log-Likelihood of a VLMC
#'
#' This function evaluates the log-likelihood of a VLMC fitted on a discrete time series.
#' When the optional argument \code{newdata} is provided, the function evaluates instead the
#' log-likelihood for this (new) discrete time series.
#'
#' @param vlmc the vlmc representation.
#' @param newdata an optional discrete time series.
#' @param ... additional parameters for loglikelihood.
#'
#' @return the log-likelihood of the VLMC with a nobs attribute that accounts for the number of data included in the likelihood calculation.
#' @seealso [stats::logLik]
#'
#' @examples
#' # Likelihood for a fitted VLMC.
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(0,
#'             median(powerconsumption$active_power, na.rm = TRUE),
#'             max(powerconsumption$active_power, na.rm = TRUE))
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' m_nocovariate <- vlmc(dts)
#' ll <- loglikelihood(m_nocovariate)
#' ll
#' attr(ll, "nobs")
#'
#' # Likelihood for a new time series with previously fitted VLMC.
#' pc_new <- powerconsumption[powerconsumption$week == 11, ]
#' dts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
#' ll_new <- loglikelihood(m_nocovariate, newdata = dts_new)
#' ll_new
#' attributes(ll_new)
#'
#' @export
loglikelihood <- function(vlmc, newdata, ...) {
  UseMethod("loglikelihood")
}

#' @export
loglikelihood.vlmc <- function(vlmc, newdata, ...) {
  if (missing(newdata)) {
    pre_res <- rec_loglikelihood_vlmc(vlmc)
    attr(pre_res, "nobs") <- sum(vlmc$f_by)
  } else {
    nx <- to_dts(newdata, vlmc$vals)
    nvlmc <- match_ctx(vlmc, nx$ix)
    pre_res <- rec_loglikelihood_vlmc(nvlmc)
    attr(pre_res, "nobs") <- length(newdata)
  }
  pre_res
}
