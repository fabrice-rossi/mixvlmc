#' @rdname logLik.vlmc
#' @export
logLik.vlmc_cpp <- function(object, initial = c("truncated", "specific", "extended"), ...) {
  ll <- loglikelihood(object, initial = initial)
  class(ll) <- "logLik"
  ll
}

#' @rdname loglikelihood
#' @export
loglikelihood.vlmc_cpp <- function(vlmc,
                                   newdata,
                                   initial = c("truncated", "specific", "extended"),
                                   ignore,
                                   ...) {
  restore_model(vlmc)
  initial <- match.arg(initial)
  if (missing(ignore)) {
    if (initial == "truncated") {
      ignore <- depth(vlmc)
    } else {
      ignore <- 0
    }
  } else if (ignore < depth(vlmc) && initial == "truncated") {
    stop("Cannot ignore less than ", depth(vlmc), " initial observations with `truncated` likelihood")
  }
  if (missing(newdata)) {
    if (ignore > depth(vlmc)) {
      stop("Cannot ignore more than ", depth(vlmc), " initial observations without newdata")
    }
    pre_res <- vlmc$root$logLik()
    if (initial == "specific") {
      pre_res <- pre_res - vlmc$extended_ll
    } else if (ignore > 0) {
      if (ignore == depth(vlmc)) {
        delta_res <- vlmc$extended_ll
      } else {
        delta_res <- vlmc$root$loglikelihood(vlmc$ix[1:min(ignore, length(vlmc$ix))], 0, TRUE, FALSE)
      }
      pre_res <- pre_res - delta_res
    }
    attr(pre_res, "nobs") <- max(0, vlmc$data_size - ignore)
  } else {
    newdata <- convert_with_check(newdata, vlmc$vals, "newdata")
    if (ignore >= length(newdata)) {
      stop("Cannot ignore more data than the available ones")
    }
    if (initial == "extended") {
      pre_res <- vlmc$root$loglikelihood(newdata$ix, ignore, TRUE, FALSE)
    } else {
      pre_res <- vlmc$root$loglikelihood(newdata$ix, ignore, FALSE, FALSE)
    }
    attr(pre_res, "nobs") <- max(0, length(newdata) - ignore)
  }
  ctx_nb <- context_number(vlmc)
  if (initial == "extended") {
    ctx_nb <- ctx_nb + vlmc$root$count_full_nodes()
  }
  attr(pre_res, "df") <- ctx_nb * (length(vlmc$vals) - 1)
  if (initial == "specific") {
    attr(pre_res, "df") <- attr(pre_res, "df") + depth(vlmc)
  }
  attr(pre_res, "initial") <- initial
  structure(pre_res, class = c("logLikMixVLMC", "logLik"))
}
