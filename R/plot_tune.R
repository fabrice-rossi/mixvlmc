#' Plot the results of automatic (CO)VLMC complexity selection
#'
#' This function plots the results of [tune_vlmc()] or [tune_covlmc()].
#'
#' The standard plot consists in showing the evolution of the criterion
#' used to select the model ([AIC()] or [BIC()]) as a function of the
#' cut off criterion expressed in the quantile scale (the quantile is used
#' by default to offer a common default behaviour between [vlmc()] and
#' [covlmc()]). Parameters can be used to display instead the [loglikelihood()]
#' of the model (by setting `value="likelihood"`) and to use the native
#' scale for the cut off when available (by setting `cutoff="native"`).
#'
#' @section Customisation:
#' The function sets several default before calling [base::plot()], namely:
#' - `type`: "l" by default to use a line representation;
#' - `xlab`: "Cut off (quantile scale)" by default, adapted to the actual
#'  scale;
#' - `ylab`: the name of the criterion or "Log likelihood".
#'
#' These parameters can be overridden by specifying other values when calling
#' the function. All parameters specified in addition to `x`, `value` and
#' `cutoff` are passed to [base::plot()].
#'
#' @param x a `tune_vlmc` object
#' @param value the criterion to plot (default "criterion").
#' @param cutoff the scale used for the cut off criterion (default "quantile")
#' @param ... additional parameters passed to [base::plot()]
#' @returns the `tune_vlmc` object invisibly
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' tune_result <- tune_vlmc(dts)
#' ## default plot
#' plot(tune_result)
#' ## likelihood
#' plot(tune_result, value = "likelihood")
#' ## parameters overriding
#' plot(tune_result,
#'   value = "likelihood",
#'   xlab = "Cut off", type = "b"
#' )
#' @export
plot.tune_vlmc <- function(x,
                           value = c("criterion", "likelihood"),
                           cutoff = c("quantile", "native"),
                           ...) {
  value <- match.arg(value)
  cutoff <- match.arg(cutoff)
  args <- list(...)
  if (is.null(args[["type"]])) {
    args$type <- "l"
  }
  if (cutoff == "native") {
    args$x <- x$result$cutoff
    if (is.null(args[["xlab"]])) {
      args$xlab <- "Cut off (native scale)"
    }
  } else {
    args$x <- x$result$alpha
    if (is.null(args[["xlab"]])) {
      args$xlab <- "Cut off (quantile scale)"
    }
  }
  if (value == "likelihood") {
    args$y <- x$results$loglikelihood
    if (is.null(args[["ylab"]])) {
      args$ylab <- "Log likelihood"
    }
  } else {
    args$y <- x$result[[x$criterion]]
    if (is.null(args[["ylab"]])) {
      args$ylab <- x$criterion
    }
  }
  do.call(plot, args)
  invisible(x)
}

#' @rdname plot.tune_vlmc
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 10:12, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' dts_best_model_tune <- tune_covlmc(dts, dts_cov, criterion = "AIC")
#' plot(dts_best_model_tune)
#' plot(dts_best_model_tune, value = "likelihood")
#'
#' @export
plot.tune_covlmc <- function(x,
                             value = c("criterion", "likelihood"),
                             cutoff = c("quantile", "native"),
                             ...) {
  cutoff <- match.arg(cutoff)
  if (cutoff == "native") {
    stop("native scale is not supported by covlmc objects")
  }
  do.call(plot.tune_vlmc, c(list(x = x, value = value, cutoff = cutoff), list(...)))
}
