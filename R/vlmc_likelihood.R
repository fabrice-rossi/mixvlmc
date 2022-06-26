dispatch_in_ctx_tree <- function(tree, x) {
  recurse_dispatch <- function(tree, x, nb_vals, d, from, f_by) {
    if (is.null(tree$children)) {
      list(f_by = f_by)
    } else {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]]) > 0 && length(tree$children[[v]]) > 0) {
          children[[v]] <- recurse_dispatch(tree$children[[v]], x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
          nb_children <- nb_children + 1
        } else {
          children[[v]] <- list()
        }
      }
      if (nb_children > 0) {
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

local_loglikelihood <- function(counts) {
  sc <- sum(counts)
  if (sc > 0) {
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
