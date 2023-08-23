#' @inherit logLik.vlmc
#' @export
logLik.vlmc_cpp <- function(object, initial = c("truncated", "specific", "extended"), ...) {
  ll <- loglikelihood(object, initial)
  class(ll) <- "logLik"
  ll
}

#' @inherit loglikelihood
#' @export
loglikelihood.vlmc_cpp <- function(vlmc,
                                   initial = c("truncated", "specific", "extended"),
                                   newdata, ...) {
  if (extptr_is_null(vlmc$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  initial <- match.arg(initial)
  if (missing(newdata)) {
    pre_res <- vlmc$root$logLik()
    if (initial != "extended" && depth(vlmc) != 0) {
      pre_res <- pre_res - vlmc$extended_ll
    }
    attr(pre_res, "nobs") <- vlmc$data_size
    if (initial == "truncated") {
      attr(pre_res, "nobs") <- max(0, attr(pre_res, "nobs") - depth(vlmc))
    }
  } else {
    assertthat::assert_that((typeof(newdata) == typeof(vlmc$vals)) && (class(newdata) == class(vlmc$vals)),
      msg = "newdata is not compatible with the model state space"
    )
    nx <- to_dts(newdata, vlmc$vals)
    if (initial == "extended") {
      pre_res <- vlmc$root$loglikelihood(nx$ix, TRUE, FALSE)
    } else {
      pre_res <- vlmc$root$loglikelihood(nx$ix, FALSE, FALSE)
    }
    if (initial == "truncated") {
      attr(pre_res, "nobs") <- max(0, length(newdata) - depth(vlmc))
    } else {
      attr(pre_res, "nobs") <- length(newdata)
    }
  }
  ctx_nb <- context_number(vlmc)
  if (initial == "specific") {
    ctx_nb <- ctx_nb + depth(vlmc)
  } else if (initial == "extended") {
    ctx_nb <- ctx_nb + vlmc$root$count_full_nodes()
  }
  attr(pre_res, "df") <- ctx_nb * (length(vlmc$vals) - 1)
  attr(pre_res, "initial") <- initial
  structure(pre_res, class = c("logLikMixVLMC", "logLik"))
}
