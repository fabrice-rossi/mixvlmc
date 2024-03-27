rec_loglikelihood_covlmc <- function(tree) {
  if (is.null(tree)) {
    0
  } else if (is.null(tree$children)) {
    if (is.null(tree$model)) {
      0
    } else {
      tree$model$likelihood
    }
  } else {
    ## take care of the local model
    sub_ll <- 0
    for (v in seq_along(tree$children)) {
      sub_ll <- sub_ll + rec_loglikelihood_covlmc(tree$children[[v]])
    }
    if (is.null(tree[["merged_model"]])) {
      sub_ll
    } else {
      sub_ll <- sub_ll + tree$merged_model$likelihood
      sub_ll
    }
  }
}

rec_loglikelihood_covlmc_newdata <- function(tree, d, nb_vals, y, cov, verbose = FALSE) {
  if (is.null(tree)) {
    return(0)
  }
  sub_ll <- 0
  if (!is.null(tree$children)) {
    ## recursive part
    for (v in seq_along(tree$children)) {
      sub_ll <- sub_ll + rec_loglikelihood_covlmc_newdata(tree$children[[v]], d + 1, nb_vals, y, cov, verbose)
    }
  }
  ## local model
  if (is.null(tree$children) && !is.null(tree$model)) {
    glmdata <- prepare_glm(cov, tree$match, tree$model$hsize, y, from = d - tree$model$hsize)
    if (length(glmdata$target) > 0) {
      res <- glm_likelihood(tree$model$model, glmdata$local_mm, glmdata$target)
    } else {
      res <- 0
    }
    if (verbose) { # nocov start
      print("Leaf")
      print(all.equal(glmdata$target, tree$model$data$target))
      print(stats::logLik(tree$model$model))
      print(paste(res, tree$model$likelihood))
      if (tree$model$hsize > 0) {
        print(utils::head(tree$model$data))
        print(utils::head(glmdata$local_mm))
      }
      if (!isTRUE(all.equal(res, tree$model$likelihood))) {
        print("not the same likelihoods")
      }
    } # nocov end
    ## not recursive part, direct result
    return(res)
  }
  ## take care of the merged model if there is one
  if (!is.null(tree[["merged_model"]])) {
    ## we need to find the matched data
    mm_match <- c()
    if (verbose) { # nocov start
      print("Merged model")
      print(paste("Keeping", paste(tree$merged, collapse = " ")))
    } # nocov end
    for (v in tree$merged) {
      if (verbose) { # nocov start
        print(1 + tree$children[[v]]$match)
      } # nocov end
      mm_match <- c(mm_match, 1 + tree$children[[v]]$match)
    }
    if (verbose) { # nocov start
      print(mm_match)
    } # nocov end
    if (length(mm_match) > 0) {
      ## prepare the data
      glmdata <- prepare_glm(cov, mm_match, tree$merged_model$hsize, y, from = d - tree$merged_model$hsize)
      if (verbose) { # nocov start
        print(utils::head(glmdata$local_mm))
        print(utils::head(tree$merged_model$data$local_mm))
        print(length(mm_match))
        print(nrow(tree$merged_model$data$local_mm))
        if (length(mm_match) != nrow(tree$merged_model$data$local_mm)) {
          print("not the same size")
        }
      } # nocov end
      ## update the values
      if (length(glmdata$target) > 0) {
        merged_ll <- glm_likelihood(tree$merged_model$model, glmdata$local_mm, glmdata$target)
        sub_ll <- sub_ll + merged_ll
        if (verbose) { # nocov start
          print(paste(merged_ll, tree$merged_model$likelihood))
        } # nocov end
      }
    }
  }
  ## we take care finally of the extended model if there is one
  if (!is.null(tree[["extended_model"]])) {
    ## check for an extended model
    ## we need to find the matched data
    if (d == 0) {
      ## root
      ## match only the first data point
      glmdata <- prepare_glm(cov, 0, 0, y, from = 0)
      e_ll <- glm_likelihood(tree$extended_model$model, glmdata$local_mm, glmdata$target)
      if (verbose) { # nocov start
        cat("root", e_ll, "\n")
      } # nocov end
    } else {
      mm_match <- tree$match
      for (v in seq_along(tree$children)) {
        sub_match <- tree$children[[v]]$match + 1
        mm_match <- setdiff(mm_match, sub_match)
      }
      if (length(mm_match > 0)) {
        glmdata <- prepare_glm(cov, mm_match, tree$extended_model$hsize, y,
          from = d - tree$extended_model$hsize
        )
        e_ll <- glm_likelihood(tree$extended_model$model, glmdata$local_mm, glmdata$target)
      } else {
        e_ll <- 0
      }
    }
    sub_ll <- sub_ll + e_ll
  }
  sub_ll
}

#' Log-Likelihood of a VLMC with covariates
#'
#' This function evaluates the log-likelihood of a VLMC with covariates
#' fitted on a discrete time series.
#'
#' @param object the covlmc representation.
#' @inherit logLik.vlmc
#' @examples
#'
#' ## Likelihood for a fitted VLMC with covariates.
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' ll <- logLik(m_cov)
#' attributes(ll)
#'
#' @export
logLik.covlmc <- function(object, initial = c("truncated", "specific", "extended"), ...) {
  ll <- loglikelihood(object, initial = initial)
  class(ll) <- "logLik"
  ll
}

#' Log-Likelihood of a VLMC with covariates
#'
#' This function evaluates the log-likelihood of a VLMC with covariates fitted
#' on a discrete time series. When the optional arguments `newdata` is
#' provided, the function evaluates instead the log-likelihood for this (new)
#' discrete time series on the new covariates which must be provided through the
#' `newcov` parameter.
#'
#' The definition of the likelihood function depends on the value of the
#' `initial` parameters, see the section below as well as the dedicated
#' vignette: `vignette("likelihood", package = "mixvlmc")`.
#'
#' @param vlmc the covlmc representation.
#' @param newcov an optional data frame with the new values for the covariates.
#' @inherit loglikelihood
#' @examples
#'
#' ## Likelihood for a fitted VLMC with covariates.
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' ll <- loglikelihood(m_cov)
#' ll
#' attr(ll, "nobs")
#'
#' ## Likelihood for new time series and covariates with previously
#' ## fitted VLMC with covariates
#' pc_new <- powerconsumption[powerconsumption$week == 11, ]
#' rdts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
#' rdts_cov_new <- data.frame(day_night = (pc_new$hour >= 7 & pc_new$hour <= 17))
#' ll_new <- loglikelihood(m_cov, newdata = rdts_new, newcov = rdts_cov_new)
#' ll_new
#' attributes(ll_new)
#'
#' @export
loglikelihood.covlmc <- function(vlmc, newdata,
                                 initial = c("truncated", "specific", "extended"),
                                 ignore, newcov, ...) {
  initial <- match.arg(initial)
  if (missing(ignore)) {
    if (initial == "truncated") {
      ignore <- depth(vlmc)
    } else {
      ignore <- 0
    }
  } else if (ignore < depth(vlmc) && initial == "truncated") {
    stop("Cannot ignore less than ", depth(vlmc), " initial observations with `truncated` likelihood")
  }
  if (missing(newdata)) {
    if (ignore > depth(vlmc)) {
      stop("Cannot ignore more than ", depth(vlmc), " initial observations without newdata")
    }
    assertthat::assert_that(missing(newcov),
      msg = "Cannot specify new covariate values (newcov) without new data (newdata)"
    )
    data_size <- vlmc$data_size
    ## in this case, we have directly the truncated/specific LL
    res <- rec_loglikelihood_covlmc(vlmc)
    ignore_counts <- ignore
    if (initial == "extended") {
      if (depth(vlmc) > 0) {
        ## add the full extended match
        res <- res + vlmc$extended_ll
      }
      if (ignore > 0) {
        ## remove the ignored data
        icovlmc <- match_ctx(vlmc, vlmc$ix[1:min(ignore, length(vlmc$ix))])
        delta_res <- rec_loglikelihood_covlmc_newdata(
          icovlmc, 0, length(vlmc$vals),
          vlmc$ix[1:min(ignore, length(vlmc$ix))],
          vlmc$icov[1:min(ignore, length(vlmc$ix)), , drop = FALSE]
        )
        res <- res - delta_res
      }
    }
  } else {
    if (isTRUE(vlmc$trimmed == "full")) {
      stop("loglikelihood calculation for new data is not supported by fully trimmed covlmc")
    }
    newdata <- convert_with_check(newdata, vlmc$vals, "newdata")
    assertthat::assert_that(!missing(newcov),
      msg = "Need new covariate values (newcov) with new data (newdata)"
    )
    if (ignore >= length(newdata)) {
      stop("Cannot ignore more data than the available ones")
    }
    assertthat::assert_that(is.data.frame(newcov))
    assertthat::assert_that(nrow(newcov) == length(newdata))
    data_size <- length(newdata)
    newcov <- validate_covariate(vlmc, newcov)
    ncovlmc <- match_ctx(vlmc, newdata$ix, keep_match = TRUE)
    if (length(vlmc$vals) > 2) {
      nx <- newdata$fx
    } else {
      nx <- newdata$ix
    }
    ignore_counts <- ignore
    if (initial == "specific" && ignore < depth(vlmc)) {
      ignore <- depth(vlmc)
    }
    res <- rec_loglikelihood_covlmc_newdata(ncovlmc, 0, length(vlmc$vals), nx, newcov)
    if (ignore > 0) {
      icovlmc <- match_ctx(vlmc, newdata$ix[1:min(ignore, length(newdata))], keep_match = TRUE)
      delta_res <- rec_loglikelihood_covlmc_newdata(
        icovlmc, 0, length(vlmc$vals),
        nx[1:min(ignore, length(newdata))],
        newcov[1:min(ignore, length(newdata)), , drop = FALSE]
      )
      res <- res - delta_res
    }
  }
  attr(res, "nobs") <- max(0, data_size - ignore_counts)
  max_depth <- depth(vlmc)
  if (initial == "truncated") {
    attr(res, "df") <- count_parameters(vlmc, FALSE)
  } else if (initial == "specific") {
    attr(res, "df") <- count_parameters(vlmc, FALSE) + max_depth
  } else {
    attr(res, "df") <- count_parameters(vlmc, TRUE)
  }
  attr(res, "initial") <- initial
  structure(res, class = c("logLikMixVLMC", "logLik"))
}
