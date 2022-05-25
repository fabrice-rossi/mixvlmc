

grow_ctx_tree <- function(x, vals, min_size, max_depth, covsize = 0, keep_match = FALSE, all_children = FALSE) {
  recurse_ctx_tree <- function(x, nb_vals, d, from, f_by) {
    if (d < max_depth) {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]]) >= min_size * (1 + covsize * (d + 1))) {
          children[[v]] <- recurse_ctx_tree(x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
          nb_children <- nb_children + 1
        } else {
          children[[v]] <- list()
        }
      }
      result <- list()
      if (nb_children == nb_vals | (!all_children & nb_children > 0)) {
        result$children <- children
      }
      result$f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    } else {
      result <- list(f_by = f_by)
      if (keep_match) {
        result$match <- from
      }
      result
    }
  }
  pre_res <- recurse_ctx_tree(x, length(vals), 0, NULL, table(x))
  new_ctx_tree(vals, pre_res, compute_stats = FALSE)
}

kl_div <- function(p, q) {
  pratio <- p / q
  pratio <- ifelse(p == q, 1, pratio) ## handles p=q=0
  sum(p * ifelse(pratio > 0, log(pratio), 0))
}

prune_ctx_tree <- function(tree, alpha = 0.05, verbose = FALSE) {
  recurse_prune_kl_ctx_tree <- function(tree, p_probs, ctx, K) {
    c_probs <- tree$f_by / sum(tree$f_by)
    if (!is.null(tree$children)) {
      subtrees <- vector(mode = "list", length(tree$children))
      nb_pruned <- 0
      for (v in seq_along(tree$children)) {
        subtrees[[v]] <- recurse_prune_kl_ctx_tree(tree$children[[v]], c_probs, c(ctx, v - 1), K)
        if (!is.null(subtrees[[v]]$kl)) {
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
  K <- stats::qchisq(alpha, df = length(tree$vals) - 1, lower.tail = FALSE) / 2
  pre_res <- recurse_prune_kl_ctx_tree(tree, tree$f_by / sum(tree$f_by), c(), K)
  if (!is.null(pre_res$kl)) {
    # empty result
    pre_res <- new_ctx_tree(tree$vals)
    pre_res$f_by <- tree$f_by
    pre_res
  } else {
    ## compute stats
    new_ctx_tree(pre_res$vals, pre_res)
  }
}

#' Fit a Variable Length Markov Chain (VLMC)
#'
#' This function fits a  Variable Length Markov Chain (VLMC) to a discrete time series.
#'
#' @param x a discrete time series; can be numeric, character or factor.
#' @param alpha number in (0,1) (default: 0.05) cut off value in the pruning phase.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'  a context in the growing phase of the context tree.
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'  growing phase of the context tree.
#' @return a fitted context tree
#'
#' @export
vlmc <- function(x, alpha = 0.05, min_size = 2, max_depth = 100) {
  # data conversion
  if (is.character(x) || is.numeric(x)) {
    fx <- as.factor(x)
  } else if (is.factor(x)) {
    fx <- x
  } else {
    stop(paste("x is not character, numeric or factor, but", class(x)))
  }
  vals <- levels(fx)
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  ix <- as.numeric(fx) - 1
  ctx_tree <- grow_ctx_tree(ix, vals, min_size = min_size, max_depth = max_depth)
  pruned_tree <- prune_ctx_tree(ctx_tree, alpha = alpha)
  pruned_tree
}
