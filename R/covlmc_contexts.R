#' Contexts number of a VLMC with covariates
#'
#' This function returns the total number of contexts of a VLMC with covariates.
#'
#' @param ct a fitted covlmc model.
#' @returns the number of contexts present in the VLMC with covariates.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 10)
#' # should be 3
#' context_number(m_cov)
#' @export
context_number.covlmc <- function(ct) {
  if (!is.null(ct$nb_ctx)) {
    ct$nb_ctx
  } else {
    rec_context_number(ct, count_covlmc_local_context)
  }
}

covlmc_model_extractor <- function(res, model, control) {
  cores <- NULL
  if (!is.null(control[["model"]])) {
    if (control[["model"]] == "coef") {
      cores <- data.frame(coef = I(list(model$coefficients)))
    } else {
      if (isS4(model$model)) {
        cores <- list(model = list(model$model))
        attr(cores, "row.names") <- "1"
        cores <- structure(cores, class = "data.frame")
      } else {
        cores <- data.frame(model = I(list(model$model)))
      }
    }
  }
  if (isTRUE(control$hsize)) {
    hsizes <- data.frame(hsize = model$hsize)
    cores <- flex_cbind(cores, hsizes)
  }
  if (isTRUE(control$metrics)) {
    local_metrics <- metrics_from_cm(model$metrics$conf_mat)
    local_metrics$auc <- model$metrics$auc
    local_metrics <- as.data.frame(local_metrics)
    cores <- flex_cbind(cores, local_metrics)
  }
  flex_cbind(res, cores)
}

covlmc_context_extractor <- function(path, ct, vals, control, is_leaf, p_summary) {
  if (is.null(ct[["model"]])) {
    if (!is.null(ct[["merged_model"]])) {
      res <- NULL
      for (v in ct$merged) {
        if (is.null(path)) {
          sub_path <- vals[v]
        } else {
          sub_path <- c(path, vals[v])
        }
        l_res <- frequency_context_extractor(sub_path, ct$children[[v]], vals, control, is_leaf, p_summary)
        l_res <- covlmc_model_extractor(l_res, ct$merged_model, control)
        if (isTRUE(control[["merging"]])) {
          l_res$merged <- TRUE
        }
        res <- rbind(res, l_res)
      }
      res
    } else {
      NULL
    }
  } else {
    res <- frequency_context_extractor(path, ct, vals, control, is_leaf, p_summary)
    if (!is.null(control[["model"]]) || isTRUE(control[["hsize"]]) ||
      isTRUE(control[["metrics"]]) || isTRUE(control[["merging"]])) {
      res <- covlmc_model_extractor(res, ct$model, control)
      if (isTRUE(control[["merging"]])) {
        res$merged <- FALSE
      }
    }
    res
  }
}

#' Contexts of a VLMC with covariates
#'
#' This function returns the different contexts present in a VLMC with
#' covariates, possibly with some associated data.
#'
#' @returns the list of the contexts represented in this tree or a data.frame
#'   with more content.
#' @inherit contexts.ctx_tree
#' @param model specifies whether to include the model associated to a each
#'   context. The default result with `model=NULL` does not include any model.
#'   Setting `model` to `"coef"` adds the coefficients of the models in a `coef`
#'   column, while `"full"` include the models themselves (as R objects) in a
#'   `model` column.
#' @param hsize if TRUE, adds a `hsize` column to the result data frame that
#'   gives for each context the size of the history of covariates used by the model.
#' @param ct a fitted covlmc model.
#' @param counts specifies how the counts reported by `frequency` are computed.
#'   The default value `"desc"` includes both counts that are specific to the
#'   context (if any) and counts from the descendants of the context in the
#'   tree. When `counts = "local"` the counts include only the number of times
#'   the context appears without being the last part of a longer context.
#' @param metrics if TRUE, adds predictive metrics for each context (see [metrics()]
#'   for the definition of predictive metrics).
#' @param merging if TRUE, adds a `merged` column to the result data frame. For
#'   a normal context, the value of `merged` is FALSE. Contexts that share the same
#'   model have a TRUE `merged` value.
#' @details The result is a list of all contexts when `type="auto"` (or `type="list"`),
#'   and no details have been asked via specific parameters (e.g. by setting `model`
#'   to a non `NULL` value).
#'
#'   Other results are obtained only with `type="data.frame"` (or
#'   `type="auto"`). See [contexts.ctx_tree()] for details about the `frequency`
#'   parameter. When `model` is non `NULL`, the resulting `data.frame` contains
#'   the models associated to each context (either the full R model or its
#'   coefficients).
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(0, median(pc$active_power), max(pc$active_power))
#' dts <- cut(pc$active_power, breaks = breaks)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' contexts(m_cov, model = "coef")
#' contexts(m_cov, model = "full")
#' @export
contexts.covlmc <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, frequency = NULL,
                            positions = FALSE, counts = c("desc", "local"), model = NULL, hsize = FALSE, metrics = FALSE,
                            merging = FALSE, ...) {
  type <- match.arg(type)
  counts <- match.arg(counts)
  if (is.null(model) && !hsize && counts == "desc" && !metrics && !merging) {
    NextMethod()
  } else {
    assertthat::assert_that(type %in% c("auto", "data.frame"))
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
    }
    if (!is.null(model)) {
      assertthat::assert_that(model %in% c("coef", "full"))
      if (model == "full" && isTRUE(ct$trimmed == "full")) {
        stop("Full model extraction is not supported by fully trimmed covlmc")
      }
    }
    control <- list(
      frequency = frequency, counts = counts, model = model,
      hsize = hsize, metrics = metrics, merging = merging,
      positions = positions
    )
    preres <- contexts_extractor(ct, reverse, covlmc_context_extractor, control, no_summary)
    rownames(preres) <- 1:nrow(preres)
    preres
  }
}
