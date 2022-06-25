node_fit_glm_internal <- function(local_mm, target, dim_cov, alpha, nb_vals, return_all = FALSE) {
  if (nrow(local_mm) > 0) {
    h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = dim_cov), drop = FALSE]
    local_glm <- fit_glm(target, local_mm, nb_vals)
    H0_local_glm <- fit_glm(target, h0mm, nb_vals)
    lambda <- 2 * (stats::logLik(local_glm) - stats::logLik(H0_local_glm))
    p_value <-
      stats::pchisq(as.numeric(lambda), df = dim_cov * (nb_vals - 1), lower.tail = FALSE)
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
      data = list(local_mm = local_mm, target = target),
      model = local_glm
    )
    if (return_all) {
      list(
        p_value = p_value,
        H0_model = H0_model,
        H1_model = H1_model,
        lambda = lambda
      )
    } else {
      if (p_value > alpha) {
        results <- H0_model
      } else {
        results <- H1_model
      }
      results$p_value <- p_value
      results$lambda <- lambda
      results
    }
  } else {
    NULL
  }
}


node_fit_glm <- function(tree, d, y, covariate, alpha, nb_vals, return_all = FALSE) {
  glmdata <- prepare_glm(covariate, tree$match, d, y)
  node_fit_glm_internal(glmdata$local_mm, glmdata$target, ncol(covariate), alpha, nb_vals, return_all)
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
      p_value <- stats::pchisq(as.numeric(lambda), df = cov_dim * (nb_vals - 1), lower.tail = FALSE)
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
        model = current_model,
        p_value = p_value
      )
    }
  } else {
    model$data <- NULL
    model
  }
}

ctx_tree_fit_glm <- function(tree, y, covariate, alpha, all_models = FALSE, verbose = FALSE) {
  nb_vals <- length(tree$vals)
  recurse_ctx_tree_fit_glm <-
    function(tree, d, y, covariate) {
      if (length(tree) == 0) {
        ## dead end marker, nothing to do (should not happen)
        list()
      } else if (is.null(tree[["children"]])) {
        ## let's compute the local model and return it
        ## prunable is true as there is no sub tree
        list(model = node_fit_glm(tree, d, y, covariate, alpha, nb_vals), prunable = TRUE, f_by = tree$f_by)
      } else {
        ## the recursive part
        ## let's get the models
        submodels <-
          vector(mode = "list", length = length(tree$children))
        nb_models <- 0
        nb_children <- 0
        nb_rejected <- 0
        nb_prunable <- 0
        pr_candidates <- c()
        ll_H0 <- 0
        for (v in seq_along(tree$children)) {
          if (ctx_tree_exists(tree$children[[v]])) {
            nb_children <- nb_children + 1
            submodels[[v]] <-
              recurse_ctx_tree_fit_glm(tree$children[[v]], d + 1, y, covariate)
            if (!is.null(submodels[[v]][["model"]])) {
              nb_models <- nb_models + 1
              prunable <- submodels[[v]][["prunable"]]
              if (!is.null(prunable) && prunable) {
                if (!submodels[[v]][["model"]]$H0) {
                  nb_rejected <- nb_rejected + 1
                } else {
                  pr_candidates <- c(pr_candidates, v)
                  nb_prunable <- nb_prunable + 1
                  ll_H0 <- ll_H0 + submodels[[v]][["model"]]$likelihood
                }
              }
            }
          }
        }
        if (verbose) {
          print(paste(
            "children:", nb_children, "models:", nb_models,
            "prunable:", nb_prunable, "rejected:", nb_rejected
          ))
        }
        ## we have several different possible situations, based on the assumption
        ## that the tree is full
        ## 1) at least one H0 is rejected
        ##    - we cannot prune the corresponding submodel
        ##    - the current node is not prunable
        ##    - the non rejected H0 submodels could be merged if we have at least 2 of them,
        ##      this is possible only if nb_vals > 2
        ##    - all remaining prunable submodels can be back pruned
        ## 2) no H0 is rejected
        ##    - we can try to prune all submodels at once if they are all prunable
        ##    - if some submodels are not prunable, we can merge the prunable ones
        ##      (if we have at least 2 of them)
        ##    - the current node will be prunable if this is done

        result <- list(f_by = tree$f_by)
        ## do we need a local/merged model
        if (nb_rejected > 0) {
          need_local_model <- FALSE
          need_merged_model <- nb_prunable >= 2
        } else {
          if (nb_prunable == nb_vals) {
            need_local_model <- TRUE
            need_merged_model <- FALSE
          } else {
            need_local_model <- FALSE
            need_merged_model <- nb_prunable >= 2
          }
        }
        local_model <- NULL
        if (need_local_model) {
          ## we need a local model
          if (verbose) {
            print("fitting a local model")
          }
          local_model <- node_fit_glm(tree, d, y, covariate, alpha, nb_vals, return_all = TRUE)
          chisq_df <- (1 + ncol(covariate) * d) * ((nb_vals - 1)^2)
        }
        if (need_merged_model) {
          ## we need a merged model
          if (verbose) {
            print(paste("fitting a merged model for:", paste(pr_candidates, collapse = " ")))
          }
          ## prepare the data set
          local_mm <- submodels[[pr_candidates[1]]][["model"]]$data$local_mm
          target <- submodels[[pr_candidates[1]]][["model"]]$data$target
          for (v in pr_candidates[-1]) {
            local_mm <- rbind(local_mm, submodels[[pr_candidates[v]]][["model"]]$data$local_mm)
            target <- c(target, submodels[[pr_candidates[v]]][["model"]]$data$target)
          }
          local_model <- node_fit_glm_internal(local_mm, target, ncol(covariate), alpha, nb_vals, return_all = TRUE)
          chisq_df <- (1 + ncol(covariate) * d) * ((nb_vals - 1) * length(pr_candidates))
        }
        if (!is.null(local_model)) {
          ## let us compute the likelihood of the new model on the data used by the ones to merge/remove
          ll_model_H0 <- 0
          ll_H0 <- 0
          for (v in pr_candidates) {
            ll_model_H0 <- ll_model_H0 +
              glm_likelihood(
                local_model$H1_model$model,
                submodels[[v]][["model"]]$data$local_mm,
                submodels[[v]][["model"]]$data$target
              )
            ll_H0 <- ll_H0 + submodels[[v]][["model"]]$likelihood
          }
          lambda <- 2 * (ll_H0 - ll_model_H0)
          p_value <- stats::pchisq(as.numeric(lambda),
            df = chisq_df,
            lower.tail = FALSE
          )
          if (verbose) {
            print(paste(lambda, p_value))
          }
        }
        ## let prepare first the children to keep
        if (need_local_model) {
          if (p_value > alpha) {
            ## we remove the prunable subtrees all together
            if (verbose) {
              print("pruning the subtree")
            }
            if (local_model$p_value > alpha) {
              result$model <- local_model$H0_model
            } else {
              result$model <- local_model$H1_model
            }
            result$prunable <- TRUE
          } else {
            ## we throw away the local model and keep the children
            result$children <- submodels
            result$prunable <- FALSE
          }
        } else if (need_merged_model) {
          result$prunable <- FALSE
          if (p_value > alpha) {
            ## we merge the models
            if (verbose) {
              print("merging sub models")
            }
            for (v in pr_candidates) {
              submodels[[v]][["model"]] <- NULL
            }
            result$children <- submodels
            if (local_model$p_value > alpha) {
              result$merged_model <- local_model$H0_model
            } else {
              result$merged_model <- local_model$H1_model
            }
            result$merged <- pr_candidates
          } else {
            ## we throw away the local model and keep the children
            result$children <- submodels
          }
        } else {
          result$children <- submodels
          result$prunable <- FALSE
        }
        ## then we need to back prune each model
        if (is.null(result$merged_model)) {
          if (!is.null(result$children)) {
            if (verbose) {
              print("Trying to prune covariables")
            }
            for (v in pr_candidates) {
              result$children[[v]][["model"]] <-
                node_prune_model(result$children[[v]][["model"]], ncol(covariate), nb_vals, alpha)
            }
          }
        } else {
          if (result$merged_model$H0) {
            if (verbose) {
              print("Trying to prune the merged model covariables")
            }
            result$merged_model <- node_prune_model(result$merged_model, ncol(covariate), nb_vals, alpha)
          }
        }
        result
      }
    }
  result <- recurse_ctx_tree_fit_glm(tree, 0, y, covariate)
  result$vals <- tree$vals
  result
}

count_colvmc_local_context <- function(node) {
  if (is.null(node$children)) {
    if (is.null(node[["model"]])) {
      0
    } else {
      1
    }
  } else if (is.null(node[["merged_model"]])) {
    0
  } else {
    length(node$merged)
  }
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
  ## we enforce a full context tree (with all_children=TRUE)
  ctx_tree <- grow_ctx_tree(ix, vals,
    min_size = min_size, max_depth = max_depth,
    covsize = ncol(covariate), keep_match = TRUE, all_children = TRUE
  )
  pruned_tree <- ctx_tree_fit_glm(ctx_tree, x, covariate,
    alpha = alpha, all_models = TRUE,
    verbose = FALSE
  )
  new_ctx_tree(pruned_tree$vals, pruned_tree, count_context = count_colvmc_local_context, class = "covlmc")
}


#' @export
context_number.covlmc <- function(ct) {
  if (!is.null(ct$nb_ctx)) {
    ct$nb_ctx
  } else {
    rec_context_number(ct, count_colvmc_local_context)
  }
}

rec_covlmc_contexts <- function(path, ct, vals) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is model, then this is a context
    if (is.null(ct$model)) {
      NULL
    } else {
      list(path)
    }
  } else {
    all_ctx <- list()
    for (v in seq_along(ct$children)) {
      sub_ctx <- rec_covlmc_contexts(c(path, vals[v]), ct$children[[v]], vals)
      if (!is.null(sub_ctx)) {
        all_ctx <- c(all_ctx, sub_ctx)
      }
    }
    ## we may have merged model which corresponds to multiple contexts at once
    if (!is.null(ct$merged_model)) {
      for (v in ct$merged) {
        all_ctx <- c(all_ctx, list(c(path, vals[v])))
      }
    }
    all_ctx
  }
}


#' @export
contexts.covlmc <- function(ct) {
  preres <- rec_covlmc_contexts(c(), ct, ct$vals)
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- list()
  }
  preres
}


draw_covlmc_node <- function(node, ...) {
  if (!is.null(node$model)) {
    paste(node$model$p_value, "[", paste(round(node$model$coefficients, 2), collapse = " "), "]")
  } else {
    ""
  }
}

draw_covlmc_merged <- function(node, ...) {
  if (!is.null(node$merged_model)) {
    paste(node$merged_model$p_value, "[", paste(round(node$merged_model$coefficients, 2), collapse = " "), "]")
  } else {
    ""
  }
}

rec_draw_covlmc <- function(prefix, rank, ival, nst, ct, vals, node2txt, merged_node2txt, ...) {
  ## check for pruned leaf
  if (length(ct) > 0) {
    # first print the current content
    if (rank > 0) {
      if (nst > 1 & rank == 1) {
        local_prefix <- "+ "
      } else {
        local_prefix <- "' "
      }
      local_prefix <- paste0(local_prefix, vals[ival])
    } else {
      local_prefix <- ""
    }
    cat(paste0(prefix, local_prefix))
    if (!is.null(node2txt)) {
      cat(" (", node2txt(ct, ...), ")")
    }
    cat("\n")
    # then go down the tree
    nst <- nb_sub_tree(ct)
    if (nst > 1) {
      prefix <- paste0(prefix, "| ")
    } else {
      prefix <- paste0(prefix, "  ")
    }
    if (is.null(ct[["merged_model"]])) {
      active_children <- seq_along(ct$children)
    } else {
      active_children <- setdiff(seq_along(ct$children), ct$merged)
    }
    for (v in seq_along(active_children)) {
      rec_draw_covlmc(prefix, v, active_children[v], nst, ct$children[[active_children[v]]], vals, node2txt, merged_node2txt, ...)
    }
    if (!is.null(ct[["merged_model"]])) {
      cat(paste0(prefix, "' "))
      cat(paste(vals[ct$merged], collapse = ", "))
      if (!is.null(merged_node2txt)) {
        cat(" (", merged_node2txt(ct, ...), ")")
      }
      cat("\n")
    }
  }
}

#' @export
draw.covlmc <- function(ct, node2txt = draw_covlmc_node, ...) {
  rec_draw_covlmc("", 0, 1, length(ct$vals), ct, ct$vals, node2txt, draw_covlmc_merged, ...)
  invisible(ct)
}

rec_loglikelihood_covlmc <- function(tree) {
  if (is.null(tree)) {
    list(ll = 0, df = 0L, nobs = 0L)
  } else if (is.null(tree$children)) {
    if (is.null(tree$model)) {
      list(ll = 0, df = 0L, nobs = 0L)
    } else {
      list(
        ll = tree$model$likelihood, df = length(tree$model$coefficients),
        nobs = sum(tree$f_by)
      )
    }
  } else {
    ## take care of the local model
    sub_ll <- list(ll = 0, df = 0L, nobs = 0L)
    for (v in seq_along(tree$children)) {
      ch_ll <- rec_loglikelihood_covlmc(tree$children[[v]])
      sub_ll$ll <- sub_ll$ll + ch_ll$ll
      sub_ll$df <- sub_ll$df + ch_ll$df
      sub_ll$nobs <- sub_ll$nobs + ch_ll$nobs
    }
    if (is.null(tree[["merged_model"]])) {
      sub_ll
    } else {
      sub_ll$ll <- sub_ll$ll + tree$merged_model$likelihood
      sub_ll$df <- sub_ll$df + length(tree$megred_model$coefficients)
      sub_ll$nobs <- sub_ll$nobs + length(tree$merged_model$data$target)
      sub_ll
    }
  }
}

#' @export
logLik.covlmc <- function(object, ...) {
  pre_res <- rec_loglikelihood_covlmc(object)
  ll <- pre_res$ll
  attr(ll, "df") <- pre_res$df
  attr(ll, "nobs") <- pre_res$nobs
  class(ll) <- "logLik"
  ll
}
