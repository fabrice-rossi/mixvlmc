
#' Test if the object is a vlmc model
#'
#' This function returns \code{TRUE} for VLMC models and \code{FALSE} for other objects.
#'
#' @param x an R object.
#' @return \code{TRUE} for VLMC models
#' @export
is_vlmc <- function(x) {
  inherits(x, "vlmc")
}

assertthat::on_failure(is_vlmc) <- function(call, env) {
  paste0(deparse(call$x), " is not a vlmc")
}

draw_vlmc_node <- function(node, probs = FALSE) {
  if (probs) {
    paste(node$f_by / sum(node$f_by), collapse = ", ")
  } else {
    paste(node$f_by, collapse = ", ")
  }
}

#' @export
draw.vlmc <- function(ct, node2txt = NULL, ...) {
  if (is.null(node2txt)) {
    NextMethod(node2txt = draw_vlmc_node, ...)
  } else {
    NextMethod(node2txt = node2txt, ...)
  }
  invisible(ct)
}

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
  new_ctx_tree(vals, pre_res, compute_stats = FALSE, class = "vlmc")
}

dispatch_in_ctx_tree <- function(tree, x) {
  recurse_dispatch <- function(tree, x, nb_vals, d, from, f_by) {
    if (is.null(tree$children)) {
      list(f_by = f_by)
    } else {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]])>0 && length(tree$children[[v]]) > 0) {
          children[[v]] <- recurse_dispatch(tree$children[[v]], x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
          nb_children <- nb_children + 1
        } else {
          children[[v]] <- list()
        }
      }
      if(nb_children>0) {
      result <- list(children = children)
      } else {
        result <- list()
      }
      result$f_by <- f_by
      result
    }
  }
  recurse_dispatch(tree, x, length(tree$vals), 0, NULL, table(x))
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
    pre_res <- new_ctx_tree(tree$vals, class = "vlmc")
    pre_res$f_by <- tree$f_by
    pre_res
  } else {
    ## compute stats
    new_ctx_tree(pre_res$vals, pre_res, class = "vlmc")
  }
}

local_loglikelihood <- function(counts) {
  sc <- sum(counts)
  if(sc>0) {
    probs <- counts / sum(counts)
    sum(counts * ifelse(probs > 0, log(probs), 0))
  } else {
    0
  }
}

rec_loglikelihood <- function(tree) {
  if (is.null(tree$f_by)) {
    # place holder list
    0
  } else if (is.null(tree$children)) {
    ## simple leaf case
    local_loglikelihood(tree$f_by)
  } else {
    ## recursive case
    sub_ll <- sum(sapply(tree$children, rec_loglikelihood))
    ## is the node a valid context
    nst <- nb_sub_tree(tree)
    if (nst < length(tree$f_by)) {
      ## let us add the local contribution
      sub_trees <- sapply(tree$children, function(x) !is.null(x$f_by))
      sub_counts <- rowSums(sapply(tree$children[sub_trees], function(x) x$f_by))
      loc_counts <- tree$f_by - sub_counts
      sub_ll <- sub_ll + local_loglikelihood(loc_counts)
    }
    sub_ll
  }
}

#' @export
logLik.vlmc <- function(object, ...) {
  ll <- rec_loglikelihood(object)
  attr(ll, "df") <- object$nb_ctx * (length(object$vals) - 1)
  attr(ll, "nobs") <- sum(object$f_by)
  class(ll) <- "logLik"
  ll
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
#' @return a fitted vlmc model
#'
#' @export
vlmc <- function(x, alpha = 0.05, min_size = 2, max_depth = 100) {
  # data conversion
  nx <- to_dts(x)
  ix <- nx$ix
  vals <- nx$vals
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  ctx_tree <- grow_ctx_tree(ix, vals, min_size = min_size, max_depth = max_depth)
  pruned_tree <- prune_ctx_tree(ctx_tree, alpha = alpha)
  pruned_tree
}

#' Log-Likelihood of a VLMC
#'
#' This function evaluates the log-likelihood of a VLMC fitted on a discrete time series.
#' When the optional argument \code{x} is provided, the function evaluates instead the
#' log-likelihood for this (new) discrete time series.
#'
#' @param vlmc the vlmc representation
#' @param x an optional discrete time series
#'
#' @return the log-likelihood of the VLMC
#' @seealso [stats::logLik]
#' @export
loglikelihood <- function(vlmc, x = NULL) {
  assertthat::assert_that(is_vlmc(vlmc))
  if (is.null(x)) {
    rec_loglikelihood(vlmc)
  } else {
    nx <- to_dts(x, vlmc$vals)
    nvlmc <- dispatch_in_ctx_tree(vlmc, nx$ix)
    rec_loglikelihood(nvlmc)
  }
}
