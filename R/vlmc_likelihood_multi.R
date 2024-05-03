#' @inherit loglikelihood
#' @param newdata either a discrete time series or a list of discrete time
#'   series
#' @param weights optional weights for the time series, see details.
#' @description This function evaluates the log-likelihood of a VLMC fitted on a
#'   collection of discrete time series.
#'
#' @section Weights:
#'
#'   When specified, the weights are used to compute a weighted sum of the log
#'   likelihood of the time series. The weights are interpreted as fractional
#'   instances and the number of observations is therefore computed accordingly.
#'
#' @seealso [multi_vlmc()]
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 5:9, ]
#' powerlevels <- quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))
#' dts <- tapply(pc$active_power, pc$week, \(x) cut(x, breaks = c(0, powerlevels)))
#' model <- multi_vlmc(dts[-5], max_depth = 3)
#' loglikelihood(model, newdata = dts[[5]])
#' loglikelihood(model, newdata = dts[1:4])
#' @export
loglikelihood.multi_vlmc <- function(vlmc, newdata,
                                     initial = c("truncated", "specific", "extended"),
                                     ignore, weights = NULL, ...) {
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
    pre_res <- rec_loglikelihood_vlmc(vlmc, TRUE)
    ignored_data <- 0
    if (initial == "specific") {
      pre_res <- pre_res - vlmc$extended_ll
    } else if (ignore > 0) {
      if (ignore == depth(vlmc)) {
        ignored_data <- sum(lengths(vlmc$ix))
        delta_res <- vlmc$extended_ll
      } else {
        local_ix <- lapply(vlmc$ix, \(x) x[1:min(ignore, length(x))])
        ivlmc <- match_multi_ctx(vlmc, local_ix)
        ignored_data <- sum(lengths(local_ix))
        delta_res <- rec_loglikelihood_vlmc(ivlmc, TRUE)
      }
      pre_res <- pre_res - delta_res
    }
    attr(pre_res, "nobs") <- max(0, vlmc$data_size - ignored_data)
  } else {
    if (!is.list(newdata)) {
      return(NextMethod())
    } else {
      if (any(ignore >= lengths(newdata))) {
        stop("Cannot ignore more data than the available ones")
      }
      if (is_dts_list(newdata)) {
        ixs <- validate_multi_dts(newdata, vlmc$vals)
      } else {
        ixs <- validate_multi_vector(newdata, vlmc$vals)
      }
      nvlmc <- match_multi_ctx(vlmc, ixs$ixs, FALSE, weights)
      pre_res <- rec_loglikelihood_vlmc(nvlmc, TRUE)
      ignore_counts <- ignore
      if (initial == "specific" && ignore < depth(vlmc)) {
        ignore <- depth(vlmc)
      }
      if (ignore > 0) {
        ivlmc <- match_multi_ctx(vlmc, lapply(ixs$ixs, \(x) x[1:ignore]))
        delta_res <- rec_loglikelihood_vlmc(ivlmc, TRUE)
        pre_res <- pre_res - delta_res
      }
      attr(pre_res, "nobs") <- max(0, sum(lengths(newdata) - ignore_counts))
    }
  }
  ctx_nb <- context_number(vlmc)
  if (initial == "extended") {
    ctx_nb <- ctx_nb + count_full_nodes(vlmc)
  }
  attr(pre_res, "df") <- ctx_nb * (length(vlmc$vals) - 1L)
  if (initial == "specific") {
    attr(pre_res, "df") <- attr(pre_res, "df") + depth(vlmc)
  }
  attr(pre_res, "initial") <- initial
  structure(pre_res, class = c("logLikMixVLMC", "logLik"))
}
