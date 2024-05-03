#' Fit an optimal Variable Length Markov Chain (VLMC) on a collection of discrete time series
#'
#' This function fits a Variable Length Markov Chain (VLMC) to a collection of
#' discrete time series by optimizing an information criterion (BIC or AIC).
#'
#' This function is very similar to [tune_vlmc()] as it automates the process of
#' selecting the complexity of a VLMC with an information criterion. The only
#' difference with [tune_vlmc()] is that the VLMC is estimated on a collection of
#' discrete time series rather than on a single one. The internal estimation is
#' therefore based on [multi_vlmc()] rather than [vlmc()]: in particular
#' the time series can be weighted using the `weights` parameter.
#'
#' @inheritParams tune_vlmc
#' @param xs a list of discrete times series, preferably a `dts_list` as created
#'   by [dts_list()]
#' @param weights optional weights for the time series, see details
#' @inheritSection multi_vlmc Weights
#' @inherit tune_vlmc return
#' @seealso [multi_vlmc()], [dts_list()]
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
#' powerlevels <- quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))
#' dts <- lapply(
#'   5:7,
#'   function(x) {
#'     cut(pc$active_power[pc$week == x],
#'       breaks = c(0, powerlevels)
#'     )
#'   }
#' )
#' best_vlmc <- tune_multi_vlmc(dts, alpha_init = 0.001)
tune_multi_vlmc <- function(xs, criterion = c("BIC", "AIC"),
                            initial = c("truncated", "specific", "extended"),
                            alpha_init = NULL, cutoff_init = NULL,
                            min_size = 2L, max_depth = 10L,
                            verbose = 0,
                            save = c("best", "initial", "all"),
                            weights = NULL) {
  criterion <- match.arg(criterion)
  initial <- match.arg(initial)
  save <- match.arg(save)
  if(!is_dts_list(xs)) {
    xs <- dts_list(xs)
  }
  data_sizes <- lengths(xs)
  if (is.null(alpha_init) && is.null(cutoff_init)) {
    if (criterion == "BIC") {
      cutoff <- 0.25 * log(sum(data_sizes))
      f_criterion <- stats::BIC
    } else {
      cutoff <- 1
      f_criterion <- stats::AIC
    }
  } else {
    if (is.null(cutoff_init)) {
      if (is.null(alpha_init) || !is.numeric(alpha_init) || alpha_init <= 0 || alpha_init > 1) {
        stop("the alpha_init parameter must be in (0, 1]")
      }
      cutoff <- to_native(alpha_init, length(states(xs)))
    } else {
      ## cutoff takes precedence
      if (!is.numeric(cutoff_init) || cutoff_init < 0) {
        stop("the cutoff_init parameter must be a non negative number")
      }
      cutoff <- cutoff_init
    }
  }
  if (criterion == "BIC") {
    f_criterion <- stats::BIC
  } else {
    f_criterion <- stats::AIC
  }
  if (verbose > 0) {
    cat("Fitting a vlmc with max_depth=", max_depth, "and cutoff=", cutoff, "\n")
  }
  saved_models <- list()
  base_model <- multi_vlmc(xs,
    cutoff = cutoff, min_size = min_size,
    max_depth = max_depth,
    weights = weights
  )
  while (base_model$max_depth) {
    n_max_depth <- min(2 * max_depth, min(data_sizes) - 1)
    if (n_max_depth > max_depth) {
      if (verbose > 0) {
        cat("Max depth reached, increasing it to", n_max_depth, "\n")
      }
      max_depth <- n_max_depth
      base_model <- multi_vlmc(xs,
        cutoff = cutoff, min_size = min_size,
        max_depth = max_depth,
        weights = weights
      )
    } else {
      warning("cannot find a suitable value for max_depth")
      break
    }
  }
  if (verbose > 0) {
    cat("Pruning phase\n")
  }
  cutoffs <- cutoff(base_model, scale = "native")
  results <- data.frame(
    cutoff = c(cutoff, cutoffs),
    alpha = to_quantile(c(cutoff, cutoffs), length(states(base_model))),
    depth = rep(NA, length(cutoffs) + 1),
    nb_contexts = rep(NA, length(cutoffs) + 1),
    loglikelihood = rep(NA, length(cutoffs) + 1),
    AIC = rep(NA, length(cutoffs) + 1),
    BIC = rep(NA, length(cutoffs) + 1)
  )
  k <- 1
  model <- base_model
  best_crit <- Inf
  if (verbose > 0) {
    cat("Initial criterion =", best_crit, "\n")
  }
  if (save == "all") {
    all_models <- vector(mode = "list", length = length(cutoffs))
  }
  max_order <- depth(model)
  repeat {
    if (verbose > 0) {
      cat("Computing loglikelihood\n")
    }
    if (initial == "truncated") {
      ll <- loglikelihood(model, initial = "truncated", newdata = xs, ignore = max_order, weights = weights)
    } else {
      ll <- loglikelihood(model, initial = initial, newdata = xs, weights = weights)
    }
    crit <- f_criterion(ll)
    if (crit <= best_crit) {
      best_crit <- crit
      best_model <- model
      if (verbose > 0) {
        cat(
          "Improving criterion =", best_crit, "likelihood =", ll,
          "df =", attr(ll, "df"),
          "nobs = ", attr(ll, "nobs"), "\n"
        )
      }
    }
    results$depth[k] <- depth(model)
    results$nb_contexts[k] <- context_number(model)
    results$loglikelihood[k] <- ll
    results$AIC[k] <- stats::AIC(ll)
    results$BIC[k] <- stats::BIC(ll)
    if (k <= length(cutoffs)) {
      if (verbose > 0) {
        cat("Pruning vlmc with cutoff =", cutoffs[k], "\n")
      }
      model <- prune(model, cutoff = cutoffs[k])
      if (save == "all") {
        all_models[[k]] <- model
      }
      k <- k + 1
    } else {
      break
    }
  }
  pre_result <- list(
    best_model = best_model,
    best_ll = loglikelihood(best_model, newdata = xs, weights = weights),
    criterion = criterion,
    initial = initial,
    results = results,
    cutoffs = c(cutoff, cutoffs)
  )
  if (save == "all") {
    pre_result[["saved_models"]] <- list(initial = base_model, all = all_models)
  } else if (save == "initial") {
    pre_result[["saved_models"]] <- list(initial = base_model)
  }
  structure(pre_result, class = "tune_vlmc")
}
