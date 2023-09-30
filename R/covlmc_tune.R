#' Fit an optimal Variable Length Markov Chain with Covariates (coVLMC)
#'
#' This function fits a Variable Length Markov Chain with Covariates (coVLMC)
#' to a discrete time series coupled with a time series of covariates
#' by optimizing an information criterion (BIC or AIC).
#'
#' This function automates the process of fitting a large coVLMC to a discrete time
#' series with [covlmc()] and of pruning the tree (with [cutoff()] and [prune()])
#' to get an optimal with respect to an information criterion. To avoid missing
#' long term dependencies, the function uses the `max_depth` parameter as an initial
#' guess but then relies on an automatic increase of the value to make sure the
#' initial context tree is only limited by the `min_size` parameter. The initial
#' value of the `alpha` parameter of [covlmc()] is also set to a conservative value
#' to avoid prior simplification of the context tree.
#'
#' Once the initial coVLMC is obtained, the [cutoff()] and [prune()] functions are
#' used to build all the coVLMC models that could be generated using smaller values of
#' the alpha parameter. The best model is selected from this collection,
#' including the initial complex tree, as the one that minimizes the chosen
#' information criterion.
#'
#' @param x a discrete time series; can be numeric, character, factor and logical.
#' @param covariate a data frame of covariates.
#' @param criterion criterion used to select the best model. Either `"BIC"` (default)
#'   or `"AIC"` (see details).
#' @param initial specifies the likelihood function, more precisely the way the
#'   first few observations for which contexts cannot be calculated are integrated
#'   in the likelihood. See [loglikelihood()] for details.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context in the growing phase of the initial context tree.
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'   growing phase of the initial context tree (see details).
#' @param verbose integer >= 0 (default: 0). Verbosity level of the pruning process.
#' @param save specify which BIC models are saved during the pruning process. The default
#'   value `"best"` asks the function to keep only the best model according to
#'   the `criterion`. When `save="initial"` the function keeps *in addition* the
#'   initial (complex) model which is then pruned during the selection process.
#'   When `save="all"`, the function returns all the models considered during the
#'   selection process. See details for memory occupation.
#' @param trimming specify the type of trimming used when saving the intermediate models,
#'   see details.
#' @param best_trimming specify the type of trimming used when saving the best model
#'   and the initial one (see details).
#'
#' @returns a list with the following components:
#'
#'   - `best_model`: the optimal VLMC
#'   - `criterion`: the criterion used to select the optimal VLMC
#'   - `initial`: the likelihood function used to select the optimal VLMC
#'   - `results`: a data frame with details about the pruning process
#'   - `saved_models`: a list of intermediate coVLMCs if `save="initial"` or
#'   `save="all"`. It contains an `initial` component with the large coVLMC obtained
#'    first and an `all` component with a list of all the *other* coVLMC obtained
#'    by pruning the initial one.
#'
#' @section Memory occupation:
#'
#' `covlmc` objects tend to be large and saving all the models during the search for
#' the optimal model can lead to an unreasonable use of memory. To avoid this problem,
#' models are kept in trimmed form only using [trim.covlmc()] with `keep_model=FALSE`.
#' Both the initial model and the best one are saved untrimmed. This default
#' behaviour corresponds to `trimming="full"`. Setting `trimming="partial"` asks the function
#' to use `keep_model=TRUE` in [trim.covlmc()] for intermediate models. Finally,
#' `trimming="none"` turns off trimming, which is discouraged expected for small data sets.
#'
#' In parallel processing contexts (e.g. using [foreach::%dopar%]), the memory
#' occupation of the results can become very large as models tend to keep
#' environments attached to the formulas. In this situation, it is highly recommended
#' to trim all saved models, including the best one and the initial one. This can
#' be done via the `best_trimming` parameter whose possible values are identical
#' to the ones of `trimming`.
#'
#' @export
#' @seealso [covlmc()], [cutoff()] and [prune()]
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' dts_best_model_tune <- tune_covlmc(dts, dts_cov)
#' draw(as_covlmc(dts_best_model_tune))
tune_covlmc <- function(x, covariate, criterion = c("BIC", "AIC"),
                        initial = c("truncated", "specific", "extended"),
                        min_size = 5, max_depth = 100,
                        verbose = 0,
                        save = c("best", "initial", "all"),
                        trimming = c("full", "partial", "none"),
                        best_trimming = c("none", "partial", "full")) {
  criterion <- match.arg(criterion)
  initial <- match.arg(initial)
  best_trimming <- match.arg(best_trimming)
  save <- match.arg(save)
  criterion <- match.arg(criterion)
  trimming <- match.arg(trimming)
  if (criterion == "BIC") {
    f_criterion <- stats::BIC
  } else {
    f_criterion <- stats::AIC
  }
  alpha <- 0.5
  if (verbose > 0) {
    cat("Fitting a covlmc with max_depth=", max_depth, "and alpha=", alpha, "\n")
  }
  saved_models <- list()
  base_model <- covlmc(x, covariate, alpha = alpha, min_size = min_size, max_depth = max_depth)
  while (base_model$max_depth) {
    n_max_depth <- min(2 * max_depth, length(x) - 1)
    if (n_max_depth > max_depth) {
      if (verbose > 0) {
        cat("Max depth reached, increasing it to", n_max_depth, "\n")
      }
      max_depth <- n_max_depth
      base_model <- covlmc(x, covariate, alpha = alpha, min_size = min_size, max_depth = max_depth)
    } else {
      warning("cannot find a suitable value for max_depth")
      break
    }
  }
  results <- NULL
  model <- base_model
  max_order <- depth(model)
  best_crit <- Inf
  if (verbose > 0) {
    cat("Initial criterion=", best_crit, "\n")
  }
  if (save == "all") {
    all_models <- list()
  }
  repeat {
    if (initial == "truncated") {
      ll <- loglikelihood(model,
        newdata = x, initial = "truncated",
        ignore = max_order, newcov = covariate
      )
    } else {
      ll <- stats::logLik(model, initial = initial)
    }
    crit <- f_criterion(ll)
    if (crit <= best_crit) {
      best_crit <- crit
      best_model <- model
      if (verbose > 0) {
        cat("Improving criterion=", best_crit, "\n")
      }
    }
    a_result <- data.frame(
      alpha = alpha,
      depth = depth(model),
      nb_contexts = context_number(model),
      loglikelihood = ll,
      cov_depth = covariate_depth(model),
      AIC = stats::AIC(ll),
      BIC = stats::BIC(ll)
    )
    if (is.null(results)) {
      results <- a_result
    } else {
      results <- rbind(results, a_result)
    }
    new_alphas <- cutoff(model)
    if (is.null(new_alphas) || all(new_alphas >= alpha)) {
      break
    } else {
      alpha <- max(new_alphas[new_alphas < alpha])
      if (alpha <= 0) {
        break
      }
      if (verbose > 0) {
        cat("Pruning covlmc with alpha=", alpha, "\n")
      }
      model <- prune(model, alpha = alpha)
      if (save == "all") {
        saved_model <- model
        if (trimming == "full") {
          saved_model <- trim(model)
        } else if (trimming == "partial") {
          saved_model <- trim(model, keep_model = TRUE)
        }
        all_models <- c(all_models, list(saved_model))
      }
    }
  }
  pre_result <- list(
    best_model = best_model,
    criterion = criterion,
    initial = initial,
    results = results
  )
  if (best_trimming == "partial") {
    pre_result$best_model <- trim(best_model, keep_model = TRUE)
  } else if (best_trimming == "full") {
    pre_result$best_model <- trim(best_model)
  }
  if (save == "all") {
    pre_result[["saved_models"]] <- list(initial = base_model, all = all_models)
  } else if (save == "initial") {
    pre_result[["saved_models"]] <- list(initial = base_model)
  }
  if (!is.null(pre_result[["saved_models"]])) {
    if (best_trimming == "partial") {
      pre_result[["saved_models"]]$initial <- trim(pre_result[["saved_models"]]$initial, keep_model = TRUE)
    } else if (best_trimming == "full") {
      pre_result[["saved_models"]]$initial <- trim(pre_result[["saved_models"]]$initial)
    }
  }
  structure(pre_result, class = "tune_covlmc")
}

#' @export
print.tune_covlmc <- function(x, ...) {
  print(x$best_model)
  cat(" Selected by", x$criterion, "(")
  if (x$criterion == "BIC") {
    cat(min(x$results$BIC))
  } else {
    cat(min(x$results$AIC))
  }
  cat(") with likelihood function \"", x$initial, "\" (", sep = "")
  cat(loglikelihood(x$best_model))
  cat(")\n")
  invisible(x)
}
