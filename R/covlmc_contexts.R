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

covlmc_context_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
  if (is.null(ct[["model"]])) {
    if (!is.null(ct[["merged_model"]])) {
      res <- NULL
      for (v in ct$merged) {
        if (is.null(path)) {
          sub_path <- vals[v]
        } else {
          sub_path <- c(path, vals[v])
        }
        l_res <- frequency_context_extractor(tree, sub_path, ct$children[[v]], vals, control, is_leaf, p_summary)
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
    res <- frequency_context_extractor(tree, path, ct, vals, control, is_leaf, p_summary)
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

covlmc_node_content_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
  if (is.null(ct[["model"]])) {
    if (!is.null(ct[["merged_model"]])) {
      res <- NULL
      for (v in ct$merged) {
        if (is.null(path)) {
          sub_path <- vals[v]
        } else {
          sub_path <- c(path, vals[v])
        }
        l_res <- ct$children[[v]]
        l_res$merged <- ct$merged
        l_res$model <- ct$merged_model
        l_res <- list(new_ctx_node(sub_path, tree, l_res, TRUE,
          merged = TRUE,
          parent = ct, index = v, class = "ctx_node_covlmc"
        ))
        res <- c(res, l_res)
      }
      res
    } else {
      NULL
    }
  } else {
    list(new_ctx_node(path, tree, ct, TRUE, merged = FALSE, class = "ctx_node_covlmc"))
  }
}

#' Contexts of a VLMC with covariates
#'
#' This function returns the different contexts present in a VLMC with
#' covariates, possibly with some associated data.
#'
#' @returns A list of class `contexts` containing the contexts represented in
#'   this tree (as `ctx_node_covlmc`) or a data.frame.
#' @inherit contexts.ctx_tree
#' @param model specifies whether to include the model associated to a each
#'   context. The default result with `model=NULL` does not include any model.
#'   Setting `model` to `"coef"` adds the coefficients of the models in a `coef`
#'   column, while `"full"` include the models themselves (as R objects) in a
#'   `model` column.
#' @param hsize if TRUE, adds a `hsize` column to the result data frame that
#'   gives for each context the size of the history of covariates used by the
#'   model.
#' @param ct a fitted covlmc model.
#' @param local specifies how the counts reported by `frequency` are computed.
#'   When `local` is `FALSE` (default value) the counts include both counts that
#'   are specific to the context (if any) and counts from the descendants of the
#'   context in the tree. When `local` is `TRUE` the counts include only the
#'   number of times the context appears without being the last part of a longer
#'   context.
#' @param metrics if TRUE, adds predictive metrics for each context (see
#'   [metrics()] for the definition of predictive metrics).
#' @param merging if TRUE, adds a `merged` column to the result data frame. For
#'   a normal context, the value of `merged` is FALSE. Contexts that share the
#'   same model have a TRUE `merged` value.
#' @details The default behaviour of the function is to return a list of all the
#'   contexts using `ctx_node_covlmc` objects (as returned by
#'   [find_sequence.covlmc()]). The properties of the contexts can then be
#'   explored using adapted functions such as [counts()], [covariate_memory()],
#'   [cutoff.ctx_node()], [metrics.ctx_node()], [model()], [merged_with()] and
#'   [positions()].
#'
#'   When `sequence=TRUE` the method returns a data.frame whose first column,
#'   named `context`, contains the contexts as vectors (i.e. the value returned
#'   by `as_sequence()` applied to a `ctx_node` object). Other columns contain
#'   context specific values specified by the additional parameters. Setting any
#'   of those parameters to a value that ask for reporting information will
#'   toggle the result type of the function to `data.frame`.
#'
#'   See [contexts.ctx_tree()] for details about the `frequency` parameter. When
#'   `model` is non `NULL`, the resulting `data.frame` contains the models
#'   associated to each context (either the full R model or its coefficients).
#'   Other columns are added is the corresponding parameters are set to `TRUE`.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(0, median(pc$active_power), max(pc$active_power))
#' dts <- cut(pc$active_power, breaks = breaks)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' ## direct representation with ctx_node_covlmc objects
#' m_cov_ctxs <- contexts(m_cov)
#' m_cov_ctxs
#' sapply(m_cov_ctxs, covariate_memory)
#' sapply(m_cov_ctxs, is_merged)
#' sapply(m_cov_ctxs, model)
#' ## data.frame interface
#' contexts(m_cov, model = "coef")
#' contexts(m_cov, model = "full", hsize = TRUE)
#' @export
contexts.covlmc <- function(ct, sequence = FALSE, reverse = FALSE, frequency = NULL,
                            positions = FALSE, local = FALSE,
                            metrics = FALSE, model = NULL, hsize = FALSE,
                            merging = FALSE, ...) {
  assertthat::assert_that(rlang::is_logical(sequence))
  assertthat::assert_that(rlang::is_logical(reverse))
  assertthat::assert_that(rlang::is_logical(local))
  assertthat::assert_that(rlang::is_logical(metrics))
  assertthat::assert_that(rlang::is_logical(hsize))
  assertthat::assert_that(rlang::is_logical(merging))
  if (!is.null(frequency)) {
    assertthat::assert_that(frequency %in% c("total", "detailed"))
  }
  if (!is.null(model)) {
    assertthat::assert_that(model %in% c("coef", "full"))
  }
  wants_df <- !is.null(frequency) || positions || metrics || !is.null(model) || hsize || merging
  if (missing(sequence)) {
    sequence <- wants_df
  } else {
    if (!sequence && wants_df) {
      stop("sequence = 'FALSE' is incompatible with the other requested values")
    }
  }
  if (!sequence) {
    new_context_list(contexts_extractor(ct, reverse, covlmc_node_content_extractor, list(class = "ctx_node_covlmc")))
  } else {
    if (!is.null(model)) {
      if (model == "full" && isTRUE(ct$trimmed == "full")) {
        stop("Full model extraction is not supported by fully trimmed covlmc")
      }
    }
    control <- list(
      frequency = frequency, local = local, model = model,
      hsize = hsize, metrics = metrics, merging = merging,
      positions = positions
    )
    preres <- contexts_extractor(ct, reverse, covlmc_context_extractor, control, no_summary)
    rownames(preres) <- 1:nrow(preres)
    preres
  }
}
