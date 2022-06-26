rec_loglikelihood_covlmc <- function(tree) {
  if (is.null(tree)) {
    list(ll = 0, df = 0L, nobs = 0L)
  } else if (is.null(tree$children)) {
    if (is.null(tree$model)) {
      list(ll = 0, df = 0L, nobs = 0L)
    } else {
      list(
        ll = tree$model$likelihood, df = length(tree$model$coefficients),
        nobs = sum(tree$f_by)
      )
    }
  } else {
    ## take care of the local model
    sub_ll <- list(ll = 0, df = 0L, nobs = 0L)
    for (v in seq_along(tree$children)) {
      ch_ll <- rec_loglikelihood_covlmc(tree$children[[v]])
      sub_ll$ll <- sub_ll$ll + ch_ll$ll
      sub_ll$df <- sub_ll$df + ch_ll$df
      sub_ll$nobs <- sub_ll$nobs + ch_ll$nobs
    }
    if (is.null(tree[["merged_model"]])) {
      sub_ll
    } else {
      sub_ll$ll <- sub_ll$ll + tree$merged_model$likelihood
      sub_ll$df <- sub_ll$df + length(tree$megred_model$coefficients)
      sub_ll$nobs <- sub_ll$nobs + length(tree$merged_model$data$target)
      sub_ll
    }
  }
}

#' @export
logLik.covlmc <- function(object, ...) {
  pre_res <- rec_loglikelihood_covlmc(object)
  ll <- pre_res$ll
  attr(ll, "df") <- pre_res$df
  attr(ll, "nobs") <- pre_res$nobs
  class(ll) <- "logLik"
  ll
}
