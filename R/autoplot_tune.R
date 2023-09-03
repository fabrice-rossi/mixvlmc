#' Create a complete ggplot for the results of automatic VLMC complexity
#' selection
#'
#' This function prepares a plot of the results of [tune_vlmc()] using ggplot2.
#' The result can be passed to [print()] to display the result.
#'
#' The graphical representation proposed by this function is complete, while the
#' one produced by [plot.tune_vlmc()] is minimalistic. We use here the faceting
#' capabilities of ggplot2 to combine on a single graphical representation the
#' evolution of multiple characteristics of the VLMC during the pruning process,
#' while [plot.tune_vlmc()] shows only the selection criterion or the log
#' likelihood. Each facet of the resulting plot shows a quantity as a function
#' of the cut off expressed in quantile or native scale.
#'
#' @param object a `tune_vlmc` object
#' @param cutoff the scale used for the cut off criterion (default "quantile")
#' @param ... additional parameters (not used currently)
#' @returns a ggplot object
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 10:11, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_best_model_tune <- tune_vlmc(dts, criterion = "BIC")
#' vlmc_plot <- ggplot2::autoplot(dts_best_model_tune)
#' print(vlmc_plot)
#' ## simple post customisation
#' print(vlmc_plot + ggplot2::geom_point())
#' @export
autoplot.tune_vlmc <- function(object, cutoff = c("quantile", "native"), ...) {
  cutoff <- match.arg(cutoff)
  if (cutoff == "quantile") {
    x_lab <- "Cut off (quantile scale)"
    x_var <- "alpha"
  } else {
    x_lab <- "Cut off (native scale)"
    x_var <- "cutoff"
  }
  vars <- names(object$results)
  vars[3] <- "Depth"
  vars[4] <- "Context number"
  vars[5] <- "Log likelihood"
  names(object$results) <- vars
  res_long <- stats::reshape(object$results,
    direction = "long",
    ids = row.names(object$results),
    varying = list(vars[-(1:2)]),
    times = vars[-(1:2)],
    idvar = "id",
    timevar = "variable"
  )

  ggplot2::ggplot(res_long, ggplot2::aes(
    x = .data[[x_var]],
    y = .data[["Depth"]]
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::ylab("") +
    ggplot2::xlab(x_lab)
}

#' Create a complete ggplot for the results of automatic COVLMC complexity
#' selection
#'
#' This function prepares a plot of the results of [tune_covlmc()] using
#' ggplot2. The result can be passed to [print()] to display the result.
#'
#' The graphical representation proposed by this function is complete, while the
#' one produced by [plot.tune_covlmc()] is minimalistic. We use here the
#' faceting capabilities of ggplot2 to combine on a single graphical
#' representation the evolution of multiple characteristics of the VLMC during
#' the pruning process, while [plot.tune_covlmc()] shows only the selection
#' criterion or the log likelihood. Each facet of the resulting plot shows a
#' quantity as a function of the cut off expressed in quantile or native scale.
#'
#' @param object a `tune_civlmc` object
#' @param ... additional parameters (not used currently)
#' @returns a ggplot object
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 10:12, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' dts_best_model_tune <- tune_covlmc(dts, dts_cov, criterion = "AIC")
#' covlmc_plot <- ggplot2::autoplot(dts_best_model_tune)
#' print(covlmc_plot)
#'
autoplot.tune_covlmc <- function(object, ...) {
  vars <- names(object$results)
  vars[2] <- "Depth"
  vars[3] <- "Context number"
  vars[4] <- "Log likelihood"
  names(object$results) <- vars
  res_long <- stats::reshape(object$results,
    direction = "long",
    ids = row.names(object$results),
    varying = list(vars[-1]),
    times = vars[-1],
    idvar = "id",
    timevar = "variable"
  )

  ggplot2::ggplot(res_long, ggplot2::aes(
    x = .data[["alpha"]],
    y = .data[["Depth"]]
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::ylab("") +
    ggplot2::xlab("Cut off (quantile scale)")
}
