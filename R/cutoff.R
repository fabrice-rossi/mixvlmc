to_native <- function(quantiles, space_size) {
  stats::qchisq(quantiles, df = space_size - 1, lower.tail = FALSE) / 2
}

to_quantile <- function(natives, space_size) {
  stats::pchisq(2 * natives, df = space_size - 1, lower.tail = FALSE)
}

guaranteed_pruning <- function(raw_cutoff, space_size, scale, raw) {
  if (!raw) {
    if (length(raw_cutoff) > 1) {
      pre_res <- sqrt(raw_cutoff[-length(raw_cutoff)] * raw_cutoff[-1])
      last_val <- 2 * raw_cutoff[length(raw_cutoff)] - pre_res[length(pre_res)]
      cutoff <- c(pre_res, last_val)
    } else {
      cutoff <- after(raw_cutoff)
    }
  } else {
    cutoff <- raw_cutoff
  }
  if (scale == "native") {
    cutoff
  } else {
    to_quantile(cutoff, space_size)
  }
}

## this function returns a vector of unique values using a relaxed the notion of
## uniqueness to account for rounding errors. The function assumes its input
## to be sorted
relaxed_unique <- function(x, tol = .Machine$double.eps^0.5) {
  x[c(TRUE, abs(diff(x)) >= tol)]
}

#' Cut off values for VLMC like model
#'
#' This generic function returns one or more cut off values that are guaranteed
#' to have an effect on the `model` passed to the function when a simplification
#' procedure is applied (in general a tree pruning operation as provided by
#' [prune()]).
#'
#' The exact definition of what is a cut off value depends on the model type and
#' is documented in concrete implementation of the function.
#'
#' @param model a model.
#' @param ... additional arguments for the cutoff function implementations
#' @returns a cut off value or a vector of cut off values.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' draw(model)
#' model_cuts <- cutoff(model)
#' model_2 <- prune(model, model_cuts[2])
#' draw(model_2)
#' @seealso [prune()]
#' @export
cutoff <- function(model, ...) {
  UseMethod("cutoff")
}
