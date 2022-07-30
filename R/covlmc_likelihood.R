rec_loglikelihood_covlmc <- function(tree) {
  if (is.null(tree)) {
    list(ll = 0, df = 0L, nobs = 0L)
  } else if (is.null(tree$children)) {
    if (is.null(tree$model)) {
      list(ll = 0, df = 0L, nobs = 0L)
    } else {
      list(
        ll = tree$model$likelihood, df = length(tree$model$coefficients),
        nobs = as.integer(sum(tree$f_by))
      )
    }
  } else {
    ## take care of the local model
    sub_ll <- list(ll = 0, df = 0L, nobs = 0L)
    for (v in seq_along(tree$children)) {
      ch_ll <- rec_loglikelihood_covlmc(tree$children[[v]])
      sub_ll$ll <- sub_ll$ll + ch_ll$ll
      sub_ll$df <- sub_ll$df + ch_ll$df
      sub_ll$nobs <- sub_ll$nobs + ch_ll$nobs
    }
    if (is.null(tree[["merged_model"]])) {
      sub_ll
    } else {
      sub_ll$ll <- sub_ll$ll + tree$merged_model$likelihood
      sub_ll$df <- sub_ll$df + length(tree$megred_model$coefficients)
      sub_ll$nobs <- sub_ll$nobs + length(tree$merged_model$data$target)
      sub_ll
    }
  }
}

rec_loglikelihood_covlmc_newdata <- function(tree, d, nb_vals, y, cov, verbose = FALSE) {
  if (is.null(tree)) {
    list(ll = 0, nobs = 0L)
  } else if (is.null(tree$children)) {
    if (is.null(tree$model)) {
      list(ll = 0, nobs = 0L)
    } else {
      ## we have a true model
      glmdata <- prepare_glm(cov, tree$match, tree$model$hsize, y, from = d - tree$model$hsize)
      res <- list(
        ll = glm_likelihood(tree$model$model, glmdata$local_mm, glmdata$target),
        nobs = nrow(glmdata$local_mm)
      )
      if (verbose) {
        print(all.equal(glmdata$target, tree$model$data$target))
        print(stats::logLik(tree$model$model))
        print(res$nobs)
        print(paste(res$ll, tree$model$likelihood))
        if (tree$model$hsize > 0) {
          print(utils::head(tree$model$data))
          print(utils::head(glmdata$local_mm))
        }
      }
      res
    }
  } else {
    ## recursive call
    sub_ll <- list(ll = 0, nobs = 0L)
    for (v in seq_along(tree$children)) {
      ch_ll <- rec_loglikelihood_covlmc_newdata(tree$children[[v]], d + 1, nb_vals, y, cov, verbose)
      sub_ll$ll <- sub_ll$ll + ch_ll$ll
      sub_ll$nobs <- sub_ll$nobs + ch_ll$nobs
    }
    ## take care of the merged model if there is one
    if (is.null(tree[["merged_model"]])) {
      sub_ll
    } else {
      ## we need to find the matched data
      mm_match <- tree$match
      non_merged <- setdiff(seq_along(tree$children), tree$merged)
      if (verbose) {
        print(paste("Removing", non_merged))
      }
      for (v in non_merged) {
        if (verbose) {
          print(tree$children[[v]]$match)
        }
        mm_match <- setdiff(mm_match, 1 + tree$children[[v]]$match)
      }
      if (verbose) {
        print(mm_match)
      }
      ## prepare the data
      glmdata <- prepare_glm(cov, mm_match, tree$merged_model$hsize, y, from = d - tree$merged_model$hsize)
      if (verbose) {
        print(utils::head(glmdata$local_mm))
        print(utils::head(tree$merged_model$data$local_mm))
        print(length(mm_match))
        print(nrow(tree$merged_model$data$local_mm))
      }
      ## update the values
      sub_ll$ll <- sub_ll$ll + glm_likelihood(tree$merged_model$model, glmdata$local_mm, glmdata$target)
      sub_ll$nobs <- sub_ll$nobs + nrow(glmdata$local_mm)
      sub_ll
    }
  }
}


#' @export
logLik.covlmc <- function(object, ...) {
  pre_res <- rec_loglikelihood_covlmc(object)
  ll <- pre_res$ll
  attr(ll, "df") <- pre_res$df
  attr(ll, "nobs") <- pre_res$nobs
  class(ll) <- "logLik"
  ll
}

#' Log-Likelihood of a VLMC with covariates
#'
#' This function evaluates the log-likelihood of a VLMC with covariates fitted
#' on a discrete time series. When the optional arguments \code{newdata} is
#' provided, the function evaluates instead the log-likelihood for this (new)
#' discrete time series on the new covariates which must be provided through the
#' newcov parameter.
#' @details
#' For VLMC with covariables, the method \code{loglikelihood.covlmc} will be used. For VLMC objects, \code{loglikelihood.vlmc}
#' will instead be called. For more informations on \code{loglikelihood} methods, use \code{methods(loglikelihood)} and their associated documentation.
#'
#' @param vlmc the vlmc representation.
#' @param newdata an optional discrete time series.
#' @param newcov an optional data frame with the new values for the covariates.
#' @param ... additional parameters for loglikelihood.
#'
#' @return the log-likelihood of the VLMC with a nobs attribute that accounts
#'   for the number of data included in the likelihood calculation.
#' @seealso [stats::logLik]
#'
#' @examples
#'
#' # Likelihood for a fitted VLMC with covariates.
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(0,
#'             median(powerconsumption$active_power, na.rm = TRUE),
#'             max(powerconsumption$active_power, na.rm = TRUE))
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' ll <- loglikelihood(m_cov)
#' ll
#' attr(ll, "nobs")
#'
#' # Likelihood for new time series and covariates with previously fitted VLMC with covariates
#' pc_new <- powerconsumption[powerconsumption$week == 11, ]
#' dts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
#' dts_cov_new <- data.frame(day_night = (pc_new$hour >= 7 & pc_new$hour <= 17))
#' ll_new <- loglikelihood(m_cov, newdata = dts_new, newcov = dts_cov_new)
#' ll_new
#' attributes(ll_new)
#'
#' @export
loglikelihood.covlmc <- function(vlmc, newdata, newcov, ...) {
  if (missing(newdata)) {
    assertthat::assert_that(missing(newcov),
      msg = "Cannot specify new covariate values (newcov) without new data (newdata)"
    )
    pre_res <- rec_loglikelihood_covlmc(vlmc)
  } else {
    assertthat::assert_that(!missing(newcov),
      msg = "Need new covariate values (newcov) with new data (newdata)"
    )
    assertthat::assert_that(is.data.frame(newcov))
    assertthat::assert_that(nrow(newcov) == length(newdata))
    assertthat::assert_that(assertthat::has_name(newcov, vlmc$cov_names))
    nx <- to_dts(newdata, vlmc$vals)
    ncovlmc <- match_ctx(vlmc, nx$ix, keep_match = TRUE)
    if (length(vlmc$vals) > 2) {
      newdata <- nx$fx
    } else {
      newdata <- nx$ix
    }
    pre_res <- rec_loglikelihood_covlmc_newdata(ncovlmc, 0, length(vlmc$vals), newdata, newcov)
  }
  res <- pre_res$ll
  attr(res, "nobs") <- pre_res$nobs
  res
}
