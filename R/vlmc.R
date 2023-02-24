#' Test if the object is a vlmc model
#'
#' This function returns `TRUE` for VLMC models and `FALSE` for other objects.
#'
#' @param x an R object.
#' @return `TRUE` for VLMC models.
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' # should be true
#' is_ctx_tree(model)
#' # should be true
#' is_vlmc(model)
#' # should be false
#' is_covlmc(model)
is_vlmc <- function(x) {
  inherits(x, "vlmc")
}

assertthat::on_failure(is_vlmc) <- function(call, env) {
  paste0(deparse(call$x), " is not a vlmc")
}

kl_div <- function(p, q) {
  pratio <- p / q
  pratio <- ifelse(p == q, 1, pratio) ## handles p=q=0
  sum(p * ifelse(pratio > 0, log(pratio), 0))
}

#' Cutoff values for pruning the context tree of a VLMC
#'
#' This function returns all the cutoff values that are guaranteed to induce a
#' pruning of the context tree of a VLMC. Pruning is implemented by the [prune()] function.
#'
#' @param vlmc a fitted VLMC model.
#' @param mode specify whether the results should be "native" likelihood ratio values
#'  or expressed in a "quantile" scale of a chi-squared distribution (defaults to "quantile").
#' @param ... additional arguments for the cutoff function.
#' @return a vector of cut off values.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' draw(model)
#' model_cuts <- cutoff(model)
#' model_2 <- prune(model, model_cuts[2])
#' draw(model_2)
#' @seealso [prune()]
#' @export
cutoff <- function(vlmc, mode = c("quantile", "native"), ...) {
  UseMethod("cutoff")
}

#' @inherit cutoff
#' @export
cutoff.vlmc <- function(vlmc, mode = c("quantile", "native"), ...) {
  mode <- match.arg(mode)
  recurse_kl_cutoff <- function(vlmc, p_probs) {
    c_probs <- vlmc$f_by / sum(vlmc$f_by)
    local_kl <- kl_div(c_probs, p_probs) * sum(vlmc$f_by)
    if (!is.null(vlmc$children)) {
      child_kl <- c()
      for (v in seq_along(vlmc$children)) {
        if (length(vlmc$children[[v]]) > 0) {
          child_kl <- c(child_kl, recurse_kl_cutoff(vlmc$children[[v]], c_probs))
        }
      }
      if (local_kl > max(child_kl)) {
        c(local_kl, child_kl)
      } else {
        child_kl
      }
    } else {
      local_kl
    }
  }
  pre_result <- unique(sort(recurse_kl_cutoff(vlmc, vlmc$f_by / sum(vlmc$f_by))))
  if (mode == "native") {
    before(pre_result)
  } else {
    pre_alpha <- before(stats::pchisq(2 * pre_result, df = length(vlmc$vals) - 1, lower.tail = FALSE))
    pre_alpha[pre_alpha < 0] <- 0
    pre_alpha
  }
}

prune_ctx_tree <- function(tree, alpha = 0.05, cutoff = NULL, verbose = FALSE) {
  recurse_prune_kl_ctx_tree <- function(tree, p_probs, ctx, K) {
    c_probs <- tree$f_by / sum(tree$f_by)
    if (!is.null(tree$children)) {
      subtrees <- vector(mode = "list", length(tree$children))
      nb_pruned <- 0
      for (v in seq_along(tree$children)) {
        subtrees[[v]] <- recurse_prune_kl_ctx_tree(tree$children[[v]], c_probs, c(ctx, v - 1), K)
        if (!is.null(subtrees[[v]][["kl"]])) {
          if (subtrees[[v]]$kl < K) {
            ## let's prune it
            if (verbose) {
              cat(paste("pruning", paste(c(ctx, v - 1), sep = "", collapse = ""), subtrees[[v]]$kl), "\n")
            }
            subtrees[[v]] <- list()
            nb_pruned <- nb_pruned + 1
          } else {
            subtrees[[v]] <- subtrees[[v]]$tree
          }
        }
      }
      if (nb_pruned < length(tree$children)) {
        tree$children <- subtrees
        tree
      } else {
        tree$children <- NULL
        kl <- kl_div(c_probs, p_probs) * sum(tree$f_by)
        list(kl = kl, tree = tree)
      }
    } else {
      kl <- kl_div(c_probs, p_probs) * sum(tree$f_by)
      list(kl = kl, tree = tree)
    }
  }
  if (is.null(cutoff)) {
    K <- stats::qchisq(alpha, df = length(tree$vals) - 1, lower.tail = FALSE) / 2
  } else {
    K <- cutoff
  }
  pre_res <- recurse_prune_kl_ctx_tree(tree, tree$f_by / sum(tree$f_by), c(), K)
  if (!is.null(pre_res$kl)) {
    # empty result
    pre_res <- new_ctx_tree(tree$vals, class = "vlmc")
    pre_res$f_by <- tree$f_by
    pre_res
  } else {
    ## compute stats
    new_ctx_tree(pre_res$vals, pre_res, class = "vlmc")
  }
}

#' Prune a Variable Length Markov Chain (VLMC)
#'
#' This function prunes a VLMC.
#'
#' In general, pruning a VLMC is more efficient than consrtucting two VLMC (the
#' base one and pruned one). Up to numerical instabilities, building a VLMC with
#' a `a` cut off and then pruning it with a `b` cut off (with `a>b`) should
#' produce the same VLMC than building directly the VLMC with a `b` cut off.
#' Interesting cut off values can be extracted from a VLMC using the [cutoff()]
#' function.
#'
#' @param vlmc a fitted VLMC model.
#' @param alpha number in (0,1) (default: 0.05) cutoff value in quantile scale
#'   for pruning.
#' @param cutoff positive number: cutoff value in native (likelihood ratio)
#'   scale for pruning. Defaults to the value obtained from `alpha`. Takes
#'   precedence over `alpha` if specified.
#' @param ... additional arguments for the prune function.
#'
#' @return a pruned VLMC
#' @seealso [cutoff()]
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' base_model <- vlmc(dts, alpha = 0.1)
#' model_cuts <- cutoff(base_model)
#' pruned_model <- prune(base_model, model_cuts[3])
#' draw(pruned_model)
#' direct_simple <- vlmc(dts, alpha = model_cuts[3])
#' draw(direct_simple)
#' # pruned_model and direct_simple should be identical
#' all.equal(pruned_model, direct_simple)
prune <- function(vlmc, alpha = 0.05, cutoff = NULL, ...) {
  UseMethod("prune")
}

#' @inherit prune
#' @export
prune.vlmc <- function(vlmc, alpha = 0.05, cutoff = NULL, ...) {
  result <- prune_ctx_tree(vlmc, alpha = alpha, cutoff = cutoff)
  result$alpha <- alpha
  if (is.null(cutoff)) {
    cutoff <- stats::qchisq(alpha, df = length(vlmc$vals) - 1, lower.tail = FALSE) / 2
  }
  result$cutoff <- cutoff
  result
}

#' Fit a Variable Length Markov Chain (VLMC)
#'
#' This function fits a  Variable Length Markov Chain (VLMC) to a discrete time
#' series.
#'
#' The VLMC is built using Bühlmann and Wyner's algorithm which consists in
#' fitting a context tree (see [ctx_tree()]) to a time series and then pruning
#' it in such as way that the conditional distribution of the next state of the
#' time series given the context is significantly different from the
#' distribution given a truncated version of the context.
#'
#' The construction of the context tree is controlled by `min_size` and
#' `max_depth`, exactly as in [ctx_tree()]. Significativity is measured using a
#' likelihood ratio test (threshold can be specified in terms of the ratio
#' itself with `cutoff`) or in quantile scale with `alpha`.
#'
#' Pruning can be postpone by setting `prune=FALSE`. Using a combination of
#' [cutoff()] and [prune()], the complexity of the VLMC can then be adjusted.
#' Any VLMC model can be pruned after construction, `prune=FALSE` is a
#' convenience parameter to avoid setting `alpha=1` (which essentially prevents
#' any pruning).
#'
#' @param x a discrete time series; can be numeric, character or factor.
#' @param alpha number in (0,1) (default: 0.05) cutoff value in quantile scale
#'   in the pruning phase.
#' @param cutoff positive number: cutoff value in native (likelihood ratio)
#'   scale in the pruning phase. Defaults to the value obtained from `alpha`.
#'   Takes precedence over `alpha` is specified.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context in the growing phase of the context tree.
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'   growing phase of the context tree.
#' @param prune logical: specified whether the context tree should be pruned
#'   (default behavior).
#' @return a fitted vlmc model.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' draw(model)
#' depth(model)
#' ## reduce the detph of the model
#' shallow_model <- vlmc(dts, max_depth = 3)
#' draw(shallow_model, prob = FALSE)
#' ## improve probability estimates
#' robust_model <- vlmc(dts, min_size = 25)
#' draw(robust_model, prob = FALSE) ## show the frequencies
#' draw(robust_model)
#' @export
#' @seealso [cutoff()] and [prune()]
#' @references [Bühlmann, P. and Wyner, A. J. (1999), Variable length Markov
#'   chains. Ann. Statist. 27 (2)
#'   480-513](https://dx.doi.org/10.1214/aos/1018031204)
vlmc <- function(x, alpha = 0.05, cutoff = NULL, min_size = 2, max_depth = 100, prune = TRUE) {
  # data conversion
  nx <- to_dts(x)
  ix <- nx$ix
  vals <- nx$vals
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  ctx_tree <- grow_ctx_tree(ix, vals, min_size = min_size, max_depth = max_depth, compute_stats = !prune)
  result <- ctx_tree
  if (prune) {
    result <- prune_ctx_tree(ctx_tree, alpha = alpha, cutoff = cutoff)
  }
  result$alpha <- alpha
  if (is.null(cutoff)) {
    cutoff <- stats::qchisq(alpha, df = length(vals) - 1, lower.tail = FALSE) / 2
  }
  result$cutoff <- cutoff
  result
}

#' @export
print.vlmc <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$vals, collapse = ", ")
  ), "\n")
  cat(paste(" cutoff: ", signif(x$cutoff, 4), " (quantile: ", x$alpha, ")\n", sep = ""))
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}
