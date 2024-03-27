#' Fit an optimal Variable Length Markov Chain (VLMC)
#'
#' This function fits a Variable Length Markov Chain (VLMC) to a discrete time
#' series by optimizing an information criterion (BIC or AIC).
#'
#' This function automates the process of fitting a large VLMC to a discrete
#' time series with [vlmc()] and of pruning the tree (with [cutoff()] and
#' [prune()]) to get an optimal with respect to an information criterion. To
#' avoid missing long term dependencies, the function uses the `max_depth`
#' parameter as an initial guess but then relies on an automatic increase of the
#' value to make sure the initial context tree is only limited by the `min_size`
#' parameter. The initial value of the `cutoff` parameter of [vlmc()] is also
#' set to conservative values (depending on the criterion) to avoid prior
#' simplification of the context tree. This default value can be overridden
#' using the `cutoff_init` or `alpha_init` parameter.
#'
#' Once the initial VLMC is obtained, the [cutoff()] and [prune()] functions are
#' used to build all the VLMC models that could be generated using larger values
#' of the initial cut off parameter. The best model is selected from this
#' collection, including the initial complex tree, as the one that minimizes the
#' chosen information criterion.
#'
#' @param x a discrete time series; can be numeric, character, factor and
#'   logical.
#' @param criterion criterion used to select the best model. Either `"BIC"`
#'   (default) or `"AIC"` (see details).
#' @param initial specifies the likelihood function, more precisely the way the
#'   first few observations for which contexts cannot be calculated are
#'   integrated in the likelihood. Default to `"truncated"`. See
#'   [loglikelihood()] for details.
#' @param alpha_init if non `NULL` used as the initial cut off parameter (in
#'   quantile scale) to build the initial VLMC
#' @param cutoff_init if non `NULL` used as the initial cut off parameter to
#'   build the initial VLMC. Takes precedence over `alpha_init` if specified.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context in the growing phase of the initial context tree.
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'   growing phase of the initial context tree (see details).
#' @param backend backend "R" or "C++" (default: as specified by the
#'   "mixvlmc.backend" option). Specifies the implementation used to represent
#'   the context tree and to built it. See [vlmc()] for details.
#' @param verbose integer >= 0 (default: 0). Verbosity level of the pruning
#'   process.
#' @param save specify which BIC models are saved during the pruning process.
#'   The default value `"best"` asks the function to keep only the best model
#'   according to the `criterion`. When `save="initial"` the function keeps *in
#'   addition* the initial (complex) model which is then pruned during the
#'   selection process. When `save="all"`, the function returns all the models
#'   considered during the selection process.
#'
#' @returns a list with the following components:
#'
#'   - `best_model`: the optimal VLMC
#'   - `criterion`: the criterion used to select the optimal VLMC
#'   - `initial`: the likelihood function used to select the optimal VLMC
#'   - `results`: a data frame with details about the pruning process
#'   - `saved_models`: a list of intermediate VLMCs if `save="initial"` or
#'   `save="all"`. It contains an `initial` component with the large VLMC
#'   obtained first and an `all` component with a list of all the *other* VLMC
#'   obtained by pruning the initial one.
#'
#' @export
#' @seealso [vlmc()], [cutoff()] and [prune()]
#'
#' @examples
#' rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' tune_result <- tune_vlmc(rdts)
#' draw(tune_result$best_model)
tune_vlmc <- function(x, criterion = c("BIC", "AIC"),
                      initial = c("truncated", "specific", "extended"),
                      alpha_init = NULL, cutoff_init = NULL,
                      min_size = 2L, max_depth = 100L,
                      backend = getOption("mixvlmc.backend", "R"),
                      verbose = 0,
                      save = c("best", "initial", "all")) {
  criterion <- match.arg(criterion)
  initial <- match.arg(initial)
  backend <- match.arg(backend, c("R", "C++"))
  save <- match.arg(save)
  if (is.null(alpha_init) && is.null(cutoff_init)) {
    if (criterion == "BIC") {
      cutoff <- 0.25 * log(length(x))
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
      ## we need to compute the state model
      nx <- to_dts(x)
      cutoff <- to_native(alpha_init, length(nx$vals))
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
  base_model <- vlmc(x,
    cutoff = cutoff, min_size = min_size,
    max_depth = max_depth, backend = backend
  )
  while (base_model$max_depth) {
    n_max_depth <- min(2 * max_depth, length(x) - 1)
    if (n_max_depth > max_depth) {
      if (verbose > 0) {
        cat("Max depth reached, increasing it to", n_max_depth, "\n")
      }
      max_depth <- n_max_depth
      base_model <- vlmc(x, cutoff = cutoff, min_size = min_size, max_depth = max_depth)
    } else {
      warning("cannot find a suitable value for max_depth")
      break
    }
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
    if (initial == "truncated") {
      ll <- loglikelihood(model, initial = "truncated", newdata = x, ignore = max_order)
    } else {
      ll <- stats::logLik(model, initial = initial)
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

#' @export
print.tune_vlmc <- function(x, ...) {
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
