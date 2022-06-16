node_fit_glm <- function(tree, d, y, covariate, alpha, nb_vals, return_all = FALSE) {
  glmdata <- prepare_glm(covariate, tree$match, d, y)
  local_mm <- glmdata$local_mm
  if (nrow(local_mm) > 0) {
    target <- glmdata$target
    h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = ncol(covariate)), drop = FALSE]
    local_glm <- fit_glm(target, local_mm, nb_vals)
    H0_local_glm <- fit_glm(target, h0mm, nb_vals)
    lambda <- 2 * (stats::logLik(local_glm) - stats::logLik(H0_local_glm))
    p_value <-
      1 - stats::pchisq(as.numeric(lambda), df = ncol(covariate) * (nb_vals - 1))
    H0_model <- list(
      H0 = TRUE,
      coefficients = stats::coefficients(H0_local_glm),
      likelihood = as.numeric(stats::logLik(H0_local_glm)),
      data = list(local_mm = h0mm, target = target),
      model = H0_local_glm
    )
    H1_model <- list(
      H0 = FALSE,
      coefficients = stats::coefficients(local_glm),
      likelihood = as.numeric(stats::logLik(local_glm)),
      data = glmdata,
      model = local_glm
    )
    if (return_all) {
      list(
        p_value = p_value,
        H0_model = H0_model,
        H1_model = H1_model
      )
    } else {
      if (p_value > alpha) {
        results <- H0_model
      } else {
        results <- H1_model
      }
      results$p_value <- p_value
      results
    }
  } else {
    NULL
  }
}

ctx_tree_exists <- function(tree) {
  length(tree) > 0
}

node_prune_model <- function(model, cov_dim, nb_vals, alpha) {
  local_mm <- model$data$local_mm
  target <- model$data$target
  if (ncol(local_mm) > 1) {
    nb <- ncol(local_mm) %/% cov_dim
    current_like <- model$likelihood
    current_model <- NULL
    for (k in 1:nb) {
      h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = cov_dim * k), drop = FALSE]
      H0_local_glm <- fit_glm(target, h0mm, nb_vals)
      lambda <- 2 * (current_like - stats::logLik(H0_local_glm))
      p_value <- 1 - stats::pchisq(as.numeric(lambda), df = cov_dim * (nb_vals - 1))
      if (p_value > alpha) {
        ## H0 is not rejected
        current_like <- as.numeric(stats::logLik(H0_local_glm))
        current_model <- H0_local_glm
      } else {
        ## H0 is rejected, we break the loop
        break
      }
    }
    if (is.null(current_model)) {
      ## we keep the original model
      model$data <- NULL
      model
    } else {
      list(
        H0 = FALSE,
        coefficients = stats::coefficients(current_model),
        likelihood = current_like,
        model = current_model
      )
    }
  } else {
    model$data <- NULL
    model
  }
}

ctx_tree_fit_glm <- function(tree, y, covariate, alpha, all_models = FALSE, aggressive_pruning = FALSE, verbose = FALSE) {
  nb_vals <- length(tree$vals)
  recurse_ctx_tree_fit_glm <-
    function(tree, d, y, covariate) {
      if (length(tree) == 0) {
        ## dead end marker, nothing to do (should not happen)
        list()
      } else if (is.null(tree[["children"]])) {
        ## let's compute the local model and return it
        list(model = node_fit_glm(tree, d, y, covariate, alpha, nb_vals))
      } else {
        ## the recursive part
        ## let's get the models
        submodels <-
          vector(mode = "list", length = length(tree$children))
        nb_models <- 0
        nb_children <- 0
        for (v in seq_along(tree$children)) {
          if (ctx_tree_exists(tree$children[[v]])) {
            nb_children <- nb_children + 1
            submodels[[v]] <-
              recurse_ctx_tree_fit_glm(tree$children[[v]], d + 1, y, covariate)
            if (!is.null(submodels[[v]][["model"]])) {
              nb_models <- nb_models + 1
            }
          }
        }
        pruned <- FALSE
        result <- list()
        if (nb_models == nb_vals) {
          ## pruning is possible
          ll_H0 <- 0
          nb_rejected <- 0
          for (v in seq_along(tree$children)) {
            if (!submodels[[v]][["model"]]$H0) {
              nb_rejected <- nb_rejected + 1
            } else {
              ll_H0 <- ll_H0 + submodels[[v]][["model"]]$likelihood
            }
          }
          if (nb_rejected == 0) {
            ## Let's try to remove all the children
            ## we need a local model
            if (verbose) {
              print("fitting a local model")
            }
            model <- node_fit_glm(tree, d, y, covariate, alpha, nb_vals, return_all = TRUE)
            ## we try to remove all children
            ## we need to evaluate the likelihood of model on the data used by the submodels
            ll_model_H0 <- 0
            for (v in seq_along(tree$children)) {
              ll_model_H0 <- ll_model_H0 +
                glm_likelihood(
                  model$H1_model$model,
                  submodels[[v]][["model"]]$data$local_mm,
                  submodels[[v]][["model"]]$data$target
                )
            }
            lambda <- 2 * (ll_H0 - ll_model_H0)
            ## the df is specific to nb_vals == 2
            p_value <- 1 - stats::pchisq(as.numeric(lambda),
              df = (1 + ncol(covariate) * d)
            )
            if (verbose) {
              print(paste(lambda, p_value))
            }
            if (p_value > alpha) {
              ## we remove the children all together
              if (verbose) {
                print("pruning the tree")
              }
              pruned <- TRUE
              ## and preparing for the recursive call
              if (model$p_value > alpha) {
                result[["model"]] <- model$H0_model
              } else {
                result[["model"]] <- model$H1_model
              }
            }
          }
        }
        if (all_models || nb_children < nb_vals) {
          result[["model"]] <- node_fit_glm(tree, d, y, covariate, alpha, nb_vals)
        }
        if (nb_models > 0 & !pruned) {
          ## Let's try to prune the models
          if (nb_children < nb_vals & aggressive_pruning) {
            for (v in seq_along(submodels)) {
              if (!is.null(submodels[[v]][["model"]])) {
                if (submodels[[v]][["model"]]$H0) {
                  pruned <- TRUE
                }
              }
            }
          } else {
            for (v in seq_along(submodels)) {
              if (!is.null(submodels[[v]][["model"]])) {
                if (submodels[[v]][["model"]]$H0) {
                  ## post prune
                  if (verbose) {
                    print("Trying to prune covariables")
                  }
                  submodels[[v]][["model"]] <-
                    node_prune_model(submodels[[v]][["model"]], ncol(covariate), nb_vals, alpha)
                } else {
                  submodels[[v]][["model"]]$data <- NULL
                }
              }
            }
          }
        }
        if (!pruned) {
          result[["children"]] <- submodels
        }
        result
      }
    }
  result <- recurse_ctx_tree_fit_glm(tree, 0, y, covariate)
  result$vals <- tree$vals
  result
}

#' Fit a Variable Length Markov Chain with Covariates (coVLMC)
#'
#' This function fits a  Variable Length Markov Chain with covariates (coVLMC)
#' to a discrete time series coupled with a time series of covariates
#'
#' @param x a discrete time series; can be numeric, character or factor.
#' @param covariate a data frame of covariates
#' @param alpha number in (0,1) (default: 0.05) cut off value in the pruning phase.
#' @param min_size integer >= 1 (default: 15). Tune the minimum number of observations for
#'  a context in the growing phase of the context tree (see below for details)
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'  growing phase of the context tree.
#' @return a fitted covlmc model
#'
#' @details
#' The \code{min_size} parameter is used to compute the actual number of observations per
#' context in the growing phase of the tree. It is computed as \code{min_size*(1+ncol(covariate)*(d+1))}
#' where \code{d} is the length of the context (a.k.a. the depth in the tree).
#'
#' @export
covlmc <- function(x, covariate, alpha = 0.05, min_size = 15, max_depth = 100) {
  assertthat::assert_that(is.data.frame(covariate))
  assertthat::assert_that(nrow(covariate) == length(x))
  # data conversion
  nx <- to_dts(x)
  ix <- nx$ix
  vals <- nx$vals
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  ctx_tree <- grow_ctx_tree(ix, vals,
    min_size = min_size, max_depth = max_depth,
    covsize = ncol(covariate), keep_match = TRUE, all_children = FALSE
  )
  pruned_tree <- ctx_tree_fit_glm(ctx_tree, x, covariate,
    alpha = alpha, all_models = TRUE,
    aggressive_pruning = TRUE
  )
  new_ctx_tree(pruned_tree$vals, pruned_tree, class = "covlmc")
}

draw_covlmc_node <- function(node, ...) {
  if (!is.null(node$model)) {
    paste(node$model$p_value, "[", paste(round(node$model$coefficients, 2), collapse = " "), "]")
  } else {
    ""
  }
}

#' @export
draw.covlmc <- function(ct, node2txt = NULL, ...) {
  if (is.null(node2txt)) {
    NextMethod(node2txt = draw_covlmc_node, ...)
  } else {
    NextMethod(node2txt = node2txt, ...)
  }
  invisible(ct)
}
