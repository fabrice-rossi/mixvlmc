rec_loglikelihood_vlmc_cpp <- function(tree, ct) {
  if (is.null(ct$f_by)) {
    # place holder list
    NA
  } else if (is.null(ct$children)) {
    ## simple leaf case
    local_loglikelihood_vlmc(ct$f_by, ct$data_f_by)
  } else {
    ## recursive case
    all_ll <- rep(NA, length(ct$children))
    for (v in seq_along(ct$children)) {
      child_idx <- ct$children[[v]]
      if (!is.na(child_idx)) {
        all_ll[v] <- rec_loglikelihood_vlmc_cpp(tree, tree[[child_idx]])
      }
    }
    sub_ll <- sum(all_ll, na.rm = TRUE)
    ## is the node a valid context
    if (anyNA(all_ll)) {
      ## let us add the local contribution
      sub_trees <- which(!is.na(ct$children))
      sub_counts <- rowSums(sapply(tree[ct$children[sub_trees]], function(x) x$f_by))
      loc_counts <- ct$f_by - sub_counts
      if (is.null(ct$data_f_by)) {
        sub_ll <- sub_ll + local_loglikelihood_vlmc(loc_counts)
      } else {
        data_sub_counts <- rowSums(sapply(tree[ct$children[sub_trees]], function(x) x$data_f_by))
        data_loc_counts <- ct$data_f_by - data_sub_counts
        sub_ll <- sub_ll + local_loglikelihood_vlmc(loc_counts, data_loc_counts)
      }
    }
    sub_ll
  }
}

#' @export
logLik.vlmc_cpp <- function(object, ...) {
  if (extptr_is_null(object$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  ct_r <- object$root$representation()
  ll <- rec_loglikelihood_vlmc_cpp(ct_r, ct_r[[1]])
  attr(ll, "df") <- object$nb_ctx * (length(object$vals) - 1)
  attr(ll, "nobs") <- sum(ct_r[[1]]$f_by)
  class(ll) <- "logLik"
  ll
}
