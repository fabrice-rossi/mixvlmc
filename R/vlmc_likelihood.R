local_loglikelihood_vlmc <- function(model_counts, data_counts = NULL) {
  if (is.null(data_counts)) {
    data_counts <- model_counts
  }
  sc <- sum(model_counts)
  if (sc > 0) {
    probs <- model_counts / sc
    sum(data_counts * ifelse(probs > 0, log(probs), 0))
  } else {
    ## should never happen when called from rec_loglikelihood_vlmc
    NA
  }
}

rec_loglikelihood_vlmc <- function(tree, node_as_ctx = FALSE, verbose = FALSE) {
  if (is.null(tree$f_by)) {
    # place holder list
    NA
  } else if (is.null(tree$children)) {
    ## simple leaf case
    ll <- local_loglikelihood_vlmc(tree$f_by, tree$data_f_by)
    if (verbose) { # nocov start
      cat(
        "Leaf: ", paste(tree$f_by, collapse = " "), "->",
        paste(tree$data_f_by, collapse = " "),
        "\n"
      )
    } # nocov end
    ll
  } else {
    ## recursive case
    all_ll <- sapply(tree$children, rec_loglikelihood_vlmc, node_as_ctx, verbose)
    sub_ll <- sum(all_ll, na.rm = TRUE)
    ## is the node a valid context
    if (anyNA(all_ll) || node_as_ctx) {
      ## let us add the local contribution
      sub_trees <- sapply(tree$children, function(x) !is.null(x$f_by))
      sub_counts <- rowSums(sapply(tree$children[sub_trees], function(x) x$f_by))
      loc_counts <- tree$f_by - sub_counts
      if (is.null(tree$data_f_by)) {
        sub_ll <- sub_ll + local_loglikelihood_vlmc(tree$f_by, loc_counts)
      } else {
        data_sub_counts <- rowSums(sapply(tree$children[sub_trees], function(x) x$data_f_by))
        data_loc_counts <- tree$data_f_by - data_sub_counts
        sub_ll <- sub_ll + local_loglikelihood_vlmc(tree$f_by, data_loc_counts)
        if (verbose) { # nocov start
          cat(
            "Node: ", paste(tree$f_by, collapse = " "), "->",
            paste(data_loc_counts, collapse = " "),
            "\n"
          )
        } # nocov end
      }
    }
    sub_ll
  }
}

#' Log-Likelihood of a VLMC
#'
#' This function evaluates the log-likelihood of a VLMC fitted on a discrete
#' time series.
#'
#' @param object the vlmc representation.
#' @param initial specifies the likelihood function, more precisely the way the
#'   first few observations for which contexts cannot be calculated are
#'   integrated in the likelihood. Defaults to `"truncated"`. See
#'   [loglikelihood()] for details.
#' @param ... additional parameters for logLik.
#' @seealso [loglikelihood()]
#' @returns an object of class `logLik`. This is a number, the log-likelihood of
#'   the (CO)VLMC with the following attributes:
#'  - `df`: the number of parameters used by the VLMC for this likelihood calculation
#'  - `nobs`: the number of observations included in this likelihood calculation
#'  - `initial`: the value of the `initial` parameter used to compute this likelihood
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' m_nocovariate <- vlmc(dts)
#' ll <- logLik(m_nocovariate)
#' ll
#' attributes(ll)
#' @export
logLik.vlmc <- function(object, initial = c("truncated", "specific", "extended"), ...) {
  ll <- loglikelihood(object, initial = initial)
  class(ll) <- "logLik"
  ll
}

#' Log-Likelihood of a VLMC
#'
#' This function evaluates the log-likelihood of a VLMC fitted on a discrete time series.
#' When the optional argument `newdata` is provided, the function evaluates instead the
#' log-likelihood for this (new) discrete time series.
#'
#' The definition of the likelihood function depends on the value of the
#' `initial` parameters, see the section below as well as the dedicated
#' vignette: `vignette("likelihood", package = "mixvlmc")`.
#'
#' For VLMC objects, the method `loglikelihood.vlmc` will be used. For VLMC with covariables, `loglikelihood.covlmc`
#' will instead be called. For more informations on `loglikelihood` methods, use `methods(loglikelihood)` and their associated documentation.
#'
#' @section likelihood calculation:
#'
#' In a (CO)VLMC of [depth()]=k, we need k past values in order to compute the
#' context of a given observation. As a consequence, in a time series `x`, the
#' contexts of `x[1]` to `x[k]` are unknown. Depending on the value of `initial`
#' different likelihood functions are used to tackle this difficulty:
#' * `initial=="truncated"`: the likelihood is computed using only
#'   `x[(k+1):length(x)]`
#' * `initial=="specific"`: the likelihood is computed on the full time series
#'   using a specific context for the initial values, `x[1]` to `x[k]`. Each of
#'   the specific context is unique, leading to a perfect likelihood of 1 (0 in
#'   log scale). Thus the numerical value of the likelihood is identical as the
#'   one obtained with `initial=="truncated"` but it is computed on `length(x)`
#'   with a model with more parameters than in this previous case.
#' * `initial=="extended"` (default): the likelihood is computed on the full time series
#'   using an extended context matching for the initial values, `x[1]` to `x[k]`.
#'   This can be seen as a compromised between the two other possibilities:
#'   the relaxed context matching needs in general to turn internal nodes
#'   of the context tree into actual context, increasing the number of parameters,
#'   but not as much as with "specific". However, the likelihood of say `x[1]`
#'   with an empty context is generally not 1 and thus the full likelihood is
#'   smaller than the one computed with "specific".
#'
#' In all cases, the `ignore` first values of the time series are not included
#' in the computed likelihood, but still used to compute contexts. If `ignore`
#' is not specified, it is set to the minimal possible value, that is k for the
#' `truncated` likelihood and 0 for the other ones. If it is specified, it must
#' be larger or equal to k for `truncated`.
#'
#' See the dedicated vignette for a more mathematically oriented discussion:
#' `vignette("likelihood", package = "mixvlmc")`.
#'
#' @param vlmc the vlmc representation.
#' @param newdata an optional discrete time series.
#' @param initial specifies the likelihood function, more precisely the way the
#'   first few observations for which contexts cannot be calculated are integrated
#'   in the likelihood. Defaults to `"truncated"`. See below for details.
#' @param ignore specifies the number of initial values for which the loglikelihood
#'   will not be computed. The minimal number depends on the likelihood function as
#'   detailed below.
#' @param ... additional parameters for loglikelihood.
#'
#' @returns an object of class `logLikMixVLMC` and `logLik`. This is a number,
#' the log-likelihood of the (CO)VLMC with the following attributes:
#'  - `df`: the number of parameters used by the VLMC for this likelihood calculation
#'  - `nobs`: the number of observations included in this likelihood calculation
#'  - `initial`: the value of the `initial` parameter used to compute this likelihood
#' @seealso [stats::logLik()]
#'
#' @examples
#' ## Likelihood for a fitted VLMC.
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' m_nocovariate <- vlmc(dts)
#' ll <- loglikelihood(m_nocovariate)
#' ll
#' attr(ll, "nobs")
#' attr(ll, "df")
#'
#' ## Likelihood for a new time series with previously fitted VLMC.
#' pc_new <- powerconsumption[powerconsumption$week == 11, ]
#' dts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
#' ll_new <- loglikelihood(m_nocovariate, newdata = dts_new)
#' ll_new
#' attributes(ll_new)
#' ll_new_specific <- loglikelihood(m_nocovariate, initial = "specific", newdata = dts_new)
#' ll_new_specific
#' attributes(ll_new_specific)
#' ll_new_extended <- loglikelihood(m_nocovariate, initial = "extended", newdata = dts_new)
#' ll_new_extended
#' attributes(ll_new_extended)
#'
#' @export
loglikelihood <- function(vlmc, newdata, initial = c("truncated", "specific", "extended"), ignore, ...) {
  UseMethod("loglikelihood")
}

#' @rdname loglikelihood
#' @export
loglikelihood.vlmc <- function(vlmc, newdata, initial = c("truncated", "specific", "extended"), ignore, ...) {
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
    pre_res <- rec_loglikelihood_vlmc(vlmc, TRUE)
    if (initial == "specific") {
      ## the case of specific is slightly more complicated than the others the
      ## depth(vlmc) first values are always "ignored" but still accounted for
      ## in the data count. As ignore <= depth(vlmc) without newdata we need to
      ## remove the full correction term
      pre_res <- pre_res - vlmc$extended_ll
    } else if (ignore > 0) {
      if (ignore == depth(vlmc)) {
        delta_res <- vlmc$extended_ll
      } else {
        ivlmc <- match_ctx(vlmc, vlmc$ix[1:min(ignore, length(vlmc$ix))])
        delta_res <- rec_loglikelihood_vlmc(ivlmc, TRUE)
      }
      pre_res <- pre_res - delta_res
    }
    attr(pre_res, "nobs") <- max(0, vlmc$data_size - ignore)
  } else {
    assertthat::assert_that((typeof(newdata) == typeof(vlmc$vals)) && methods::is(newdata, class(vlmc$vals)),
      msg = "newdata is not compatible with the model state space"
    )
    if (ignore >= length(newdata)) {
      stop("Cannot ignore more data than the available ones")
    }
    nx <- to_dts(newdata, vlmc$vals)
    nvlmc <- match_ctx(vlmc, nx$ix)
    pre_res <- rec_loglikelihood_vlmc(nvlmc, TRUE)
    ignore_counts <- ignore
    if (initial == "specific" && ignore < depth(vlmc)) {
      ignore <- depth(vlmc)
    }
    if (ignore > 0) {
      ivlmc <- match_ctx(vlmc, nx$ix[1:min(ignore, length(newdata))])
      delta_res <- rec_loglikelihood_vlmc(ivlmc, TRUE)
      pre_res <- pre_res - delta_res
    }
    attr(pre_res, "nobs") <- max(0, length(newdata) - ignore_counts)
  }
  ctx_nb <- context_number(vlmc)
  if (initial == "extended") {
    ctx_nb <- ctx_nb + count_full_nodes(vlmc)
  }
  attr(pre_res, "df") <- ctx_nb * (length(vlmc$vals) - 1L)
  if (initial == "specific") {
    attr(pre_res, "df") <- attr(pre_res, "df") + depth(vlmc)
  }
  attr(pre_res, "initial") <- initial
  structure(pre_res, class = c("logLikMixVLMC", "logLik"))
}

#' @export
print.logLikMixVLMC <- function(x, ...) {
  cat(paste("'log Lik.' "), x, " (df= ", attr(x, "df"),
    ", nb obs.= ", attr(x, "nobs"), ", initial=\"",
    attr(x, "initial"), "\")\n",
    sep = ""
  )
}
