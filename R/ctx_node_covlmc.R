is_ctx_node_covlmc <- function(node) {
  methods::is(node, "ctx_node_covlmc")
}

assertthat::on_failure(is_ctx_node_covlmc) <- function(call, env) {
  paste0(deparse(call$node), " is not a ctx_node_covlmc object")
}

#' Find the node of a sequence in a COVLMC context tree
#'
#' This function checks whether the sequence `ctx` is represented in the context
#' tree of the COVLMC model `ct`. If this is the case, it returns a description
#' of the matching node, an object of class `ctx_node_covlmc`. If the sequence
#' is not represented in the tree, the function returns `NULL`.
#'
#' @export
#' @inherit find_sequence
#' @returns an object of class `ctx_node_covlmc` if the sequence `ctx` is
#'   represented in the context tree, `NULL` when this is not the case
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
#'
#' ## not in the tree
#' vals <- states(m_cov)
#' find_sequence(m_cov, c(vals[2], vals[2]))
#' ## in the tree but not a context
#' node <- find_sequence(m_cov, c(vals[1]))
#' node
#' is_context(node)
#' ## in the tree and a context
#' node <- find_sequence(m_cov, c(vals[1], vals[1]))
#' node
#' is_context(node)
#' model(node)
find_sequence.covlmc <- function(ct, ctx, reverse = FALSE, ...) {
  rlang::check_dots_empty()
  if (length(ctx) == 0) {
    if (isTRUE(ct$keep_match) && is.null(ct$match)) {
      ct$match <- 0:(ct$data_size - 1L)
    }
    new_ctx_node(ctx, ct, ct, reverse, merged = FALSE, class = "ctx_node_covlmc")
  } else {
    if (!reverse) {
      ctx <- rev(ctx)
    }
    nx <- convert_with_check(ctx, ct$vals, "ctx")
    current <- ct
    ## first part
    for (k in seq_along(ctx[-length(ctx)])) {
      if (is.null(current$children)) {
        return(NULL)
      }
      candidate <- current$children[[1L + nx$ix[k]]]
      if (is.null(candidate) || length(candidate) == 0L) {
        return(NULL)
      }
      current <- candidate
    }
    ## last value
    covlmc_child_node(ctx, ct, current, 1L + nx$ix[length(ctx)], reverse)
  }
}

## node of the k-th child of current (a node of the context tree of ct) whose
## sequence is ctx, taking merged contexts into account (NULL if the child is
## not in the tree)
covlmc_child_node <- function(ctx, ct, current, k, reverse) {
  candidate <- current$children[[k]]
  if (is.null(candidate)) {
    return(NULL)
  }
  if (is.null(candidate[["model"]])) {
    if (!is.null(candidate[["children"]])) {
      ## internal node, not a context
      new_ctx_node(ctx, ct, candidate, reverse, merged = FALSE, class = "ctx_node_covlmc")
    } else if (is.null(current[["merged_model"]])) {
      NULL
    } else {
      if (k %in% current$merged) {
        l_res <- candidate
        l_res$merged <- current$merged
        l_res$model <- current$merged_model
        new_ctx_node(ctx, ct, l_res, reverse,
          merged = TRUE,
          parent = current, index = k, class = "ctx_node_covlmc"
        )
      } else {
        NULL
      }
    }
  } else {
    new_ctx_node(ctx, ct, candidate, reverse, merged = FALSE, class = "ctx_node_covlmc")
  }
}

#' @export
#' @rdname children
children.ctx_node_covlmc <- function(node) {
  if (is.null(node$node[["children"]])) {
    list()
  } else {
    res <- vector(mode = "list", length = length(node$node[["children"]]))
    for (k in seq_along(res)) {
      if (length(node$node$children[[k]]) > 0) {
        the_ctx <- append_state(node$sequence, node$tree$vals, k)
        child <- covlmc_child_node(the_ctx, node$tree, node$node, k, node$rev)
        if (!is.null(child)) {
          res[[k]] <- child
        }
      }
    }
    names(res) <- as.character(node$tree$vals)
    res
  }
}


#' Logistic model of a COVLMC context
#'
#' This function returns a representation of the logistic model associated with
#' a COVLMC context from its node in the associated context tree.
#'
#' Full model extraction is only possible if the COVLMC model was not fully
#' trimmed (see [trim.covlmc()]). Notice that [find_sequence.covlmc()] can
#' produce nodes that are not contexts: in this case this function returns
#' `NULL`.
#'
#' @param node A `ctx_node_covlmc` object as returned by
#'   [find_sequence.covlmc()] or [contexts.covlmc()]
#' @param type specifies the model information to return, either the
#'   coefficients only (`type="coef"`, default case) or the full model object
#'   (`type="full"`)
#' @export
#' @returns if `node` is a context, the coefficients of the logistic model (as a
#'   vector or a matrix depending on the size of the state space) or a logistic
#'   model as an R object. If `node` is not a context, `NULL`.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
#' vals <- states(m_cov)
#' node <- find_sequence(m_cov, c(vals[1], vals[1]))
#' node
#' model(node)
#' model(node, type = "full")
#'
model <- function(node, type = c("coef", "full")) {
  assertthat::assert_that(is_ctx_node_covlmc(node))
  type <- match.arg(type)
  if (type == "full" && isTRUE(node$tree$trimmed == "full")) {
    stop("Full model extraction is not supported by fully trimmed covlmc")
  }
  if (!is.null(node$node[["model"]])) {
    if (type == "full") {
      node$node[["model"]]$model
    } else {
      node$node[["model"]]$coefficients
    }
  } else {
    NULL
  }
}

#' Merging status of a COVLMC context
#'
#' The function returns `TRUE` if the context represented by this node is merged
#' with at least one other context and `FALSE` if this is not the case.
#'
#' When a COVLMC is built on a time series with at least three distinct states,
#' some contexts can be merged: they use the same logistic model, leading to a
#' more parsimonious model. Those contexts are reported individually by
#' functions such as [contexts.covlmc()]. The present function can be used
#' to detect such merging, while [merged_with()] can be used to recover the
#' other contexts.
#'
#' @param node A `ctx_node_covlmc` object as returned by
#'   [find_sequence.covlmc()] or [contexts.covlmc()]
#' @seealso [merged_with()]
#' @returns `TRUE` or `FALSE`, depending on the nature of the context
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 15, ]
#' rdts <- cut(pc$active_power, breaks = c(0, 1, 2, 3, 8))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.1)
#' ctxs <- contexts(m_cov)
#' ## no merging
#' sapply(ctxs, is_merged)
is_merged <- function(node) {
  assertthat::assert_that(is_ctx_node_covlmc(node))
  node$merged
}

#' Merged contexts in a COVLMC
#'
#' The function returns `NULL` when the context represented by the `node`
#' parameter is not merged with another context (see [is_merged()]). In the
#' other case, it returns a list of contexts with which this one is merged.
#'
#' If the context is merged, the function returns a list with one value for each
#' element in the state space (see [states()]). The value is `NULL` if the
#' corresponding context is not merged with the `node` context, while it is a
#' `ctx_node_covlmc` object in the other case. A context merged with `node`
#' differs from the context represented by `node` only in its oldest value
#' (i.e. its first value in temporal order) which is used as its name in the
#' list. For instance, if the context `ABC` is merged only with `CBC` (when
#' represented in temporal ordering), then the resulting list is of the form
#' `list("A" = NULL, "B" = NULL, "C" = ctx_node_covlmc(CBC))`.
#'
#' @param node A `ctx_node_covlmc` object as returned by
#'   [find_sequence.covlmc()] or [contexts.covlmc()]
#' @seealso [is_merged()]
#' @returns `NULL` or a list of contexts merged with `node` represented by
#'   `ctx_node_covlmc` objects
#' @export
#' @examples
#' pc_week_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
#' elec <- pc_week_15_16$active_power
#' elec_rdts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
#' elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
#' elec_tune <- tune_covlmc(elec_rdts, elec_cov, min_size = 5)
#' elec_model <- prune(as_covlmc(elec_tune), alpha = 3.961e-10)
#' ctxs <- contexts(elec_model)
#' for (ctx in ctxs) {
#'   if (is_merged(ctx)) {
#'     print(ctx)
#'     cat("\nis merged with\n\n")
#'     print(merged_with(ctx))
#'   }
#' }
merged_with <- function(node) {
  assertthat::assert_that(is_ctx_node_covlmc(node))
  if (node$merged) {
    pnode <- node$parent
    pseq <- node$sequence[-length(node$sequence)]
    merged <- setdiff(node$node$merged, node$index)
    res <- vector(mode = "list", length = length(pnode[["children"]]))
    for (k in merged) {
      the_ctx <- append_state(pseq, node$tree$vals, k)
      res[[k]] <- new_ctx_node(the_ctx, node$tree, pnode$children[[k]],
        node$rev,
        merged = TRUE,
        parent = pnode, index = k,
        class = "ctx_node_covlmc"
      )
      res[[k]]$node$model <- node$node[["model"]]
      res[[k]]$node$merged <- node$node$merged
    }
    names(res) <- as.character(node$tree$vals)
    res
  } else {
    NULL
  }
}


#' Covariate memory length for a COVLMC context
#'
#' This function returns the length of the covariate memory of a COVLMC context
#' represented by a `ctx_node_covlmc` object, i.e. the number of past values of
#' the covariates used by the logistic model of the context.
#'
#' @param node A `ctx_node_covlmc` object as returned by
#'   [find_sequence.covlmc()] or [contexts.covlmc()]
#' @export
#' @seealso [covariate_depth()]
#' @returns the memory length, an integer (`NULL` if `node` is not a context)
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
#' ctxs <- contexts(m_cov)
#' ## get all the memory lengths
#' sapply(ctxs, covariate_memory)
covariate_memory <- function(node) {
  assertthat::assert_that(is_ctx_node_covlmc(node))
  node$node[["model"]]$hsize
}

#' Predictive quality metrics for a node of a COVLMC context tree
#'
#' This function computes and returns predictive quality metrics for a node
#' (`ctx_node_covlmc`) extracted from a VLMC with covariates.
#'
#' Compared to [metrics.covlmc()], this function focuses on a single context and
#' assesses the quality of its predictions, disregarding observations that have
#' other contexts. Apart from this limited scope, the function operates as
#' [metrics.covlmc()]. The node must be a context (see [is_context()]).
#'
#' @param model A `ctx_node_covlmc` object as returned by
#'   [find_sequence.covlmc()] or [contexts.covlmc()]
#' @inherit metrics
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' m_ctxs <- contexts(m_cov)
#' ## get the predictive metrics for each context
#' lapply(m_ctxs, metrics)
#' @returns an object of class `metrics.covlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#' @export
metrics.ctx_node_covlmc <- function(model, ...) {
  rlang::check_dots_empty()
  if (is.null(model$node[["model"]])) {
    stop("the node is not a context, no metrics can be computed")
  }
  res <- model$node$model$metrics
  rownames(res$conf_mat) <- model$tree$vals
  colnames(res$conf_mat) <- model$tree$vals
  res$model <- model
  structure(res, class = "metrics.covlmc")
}

