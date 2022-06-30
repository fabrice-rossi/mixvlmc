## fit a glm guaranteed to be of full rank by removing older covariates if needed
node_fit_glm_full_rank <- function(index, y, covariate, nb_vals, d) {
  glmdata <- prepare_glm(covariate, index, d, y)
  node_fit_glm_full_rank_with_data(glmdata$local_mm, d, glmdata$target, ncol(covariate), nb_vals)
}

## implementation of node_fit_glm_full_rank
node_fit_glm_full_rank_with_data <- function(local_mm, d, target, dim_cov, nb_vals) {
  if (nrow(local_mm) > 0) {
    local_glm <- fit_glm(target, local_mm, nb_vals)
    while (is_glm_low_rank(local_glm)) {
      d <- d - 1
      local_mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = dim_cov), drop = FALSE]
      local_glm <- fit_glm(target, local_mm, nb_vals)
    }
    list(
      coefficients = stats::coefficients(local_glm),
      likelihood = as.numeric(stats::logLik(local_glm)),
      data = list(local_mm = local_mm, target = target),
      model = local_glm,
      hsize = d
    )
  } else {
    NULL
  }
}

## fit a glm model using a history of size <= d
## if the model with size=d is not of full rank, a full rank model with size < d
## is constructed
## if the model with size=d is of full rank, it is compared to a model of size=d-1 (H0)
## according to a likelihood ratio test. If the difference in performances is significative
## the size=d model is returned (H0 is rejected). If it is not, the size=d-1 model
## is returned (H0 is not rejected)
## if return_all is true, both models are returned when this makes sense
node_fit_glm <- function(index, d, y, covariate, alpha, nb_vals, return_all = FALSE) {
  ## prepare the data
  glmdata <- prepare_glm(covariate, index, d, y)
  node_fit_glm_with_data(glmdata$local_mm, d, glmdata$target, ncol(covariate), alpha, nb_vals, return_all)
}

## implementation of node_fit_glm
node_fit_glm_with_data <- function(local_mm, d, target, dim_cov, alpha, nb_vals, return_all = FALSE) {
  ## compute the full rank "H1" model
  full_rank_model <- node_fit_glm_full_rank_with_data(
    local_mm,
    d,
    target,
    dim_cov, nb_vals
  )
  if (!is.null(full_rank_model)) {
    if (full_rank_model$hsize < d || d == 1) {
      ## if the full rank model does not use a size d history
      ## we do not need to build another model, backtracking will be done
      ## elsewhere (if needed)
      full_rank_model$H0 <- TRUE
      full_rank_model$p_value <- NA
      if (return_all) {
        list(
          p_value = NA,
          H0_model = full_rank_model,
          H1_model = full_rank_model,
          lambda = NA
        )
      } else {
        full_rank_model
      }
    } else {
      ## in the other case we need to look for a simpler model
      h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = dim_cov), drop = FALSE]
      H0_full_rank_model <- node_fit_glm_full_rank_with_data(
        h0mm,
        d - 1, target,
        dim_cov, nb_vals
      )
      full_rank_model$H0 <- FALSE
      H0_full_rank_model$H0 <- TRUE
      lambda <- 2 * (full_rank_model$likelihood - H0_full_rank_model$likelihood)
      df <- (full_rank_model$hsize - H0_full_rank_model$hsize) * dim_cov * (nb_vals - 1)
      p_value <-
        stats::pchisq(as.numeric(lambda), df = df, lower.tail = FALSE)
      if (return_all) {
        list(
          p_value = p_value,
          H0_model = H0_full_rank_model,
          H1_model = full_rank_model,
          lambda = lambda
        )
      } else {
        if (p_value > alpha) {
          results <- H0_full_rank_model
        } else {
          results <- full_rank_model
        }
        results$p_value <- p_value
        results$lambda <- lambda
        results
      }
    }
  } else {
    ## should not happen
    NULL
  }
}

ctx_tree_exists <- function(tree) {
  length(tree) > 0
}

node_prune_model <- function(model, cov_dim, nb_vals, alpha, keep_data = FALSE, verbose = FALSE) {
  local_mm <- model$data$local_mm
  target <- model$data$target
  if (ncol(local_mm) > 1) {
    nb <- ncol(local_mm) %/% cov_dim
    current_like <- model$likelihood
    previous_model <- model$model
    current_model <- NULL
    current_data <- NULL
    hsize <- model$hsize
    for (k in 1:nb) {
      if (verbose) {
        print(k)
      }
      h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = cov_dim * k), drop = FALSE]
      H0_local_glm <- fit_glm(target, h0mm, nb_vals)
      assertthat::assert_that(!is_glm_low_rank(H0_local_glm))
      lambda <- 2 * (current_like - stats::logLik(H0_local_glm))
      p_value <- stats::pchisq(as.numeric(lambda), df = cov_dim * (nb_vals - 1), lower.tail = FALSE)
      if (p_value > alpha) {
        ## H0 is not rejected
        current_like <- as.numeric(stats::logLik(H0_local_glm))
        current_model <- H0_local_glm
        previous_model <- H0_local_glm
        if (keep_data) {
          current_data <- list(local_mm = h0mm, target = target)
        }
        hsize <- hsize - 1
      } else {
        ## H0 is rejected, we break the loop
        if (verbose) {
          print("rejecting H0")
        }
        break
      }
    }
    if (is.null(current_model)) {
      ## we keep the original model
      if (!keep_data) {
        model$data <- NULL
      }
      model
    } else {
      list(
        H0 = FALSE,
        coefficients = stats::coefficients(current_model),
        likelihood = current_like,
        model = current_model,
        p_value = p_value,
        hsize = hsize,
        data = current_data
      )
    }
  } else {
    if (!keep_data) {
      model$data <- NULL
    }
    model
  }
}

ctx_tree_fit_glm <- function(tree, y, covariate, alpha, keep_data = FALSE, verbose = FALSE) {
  nb_vals <- length(tree$vals)
  recurse_ctx_tree_fit_glm <-
    function(tree, ctx, d, y, covariate) {
      if (length(tree) == 0) {
        ## dead end marker, nothing to do (should not happen)
        list()
      } else if (is.null(tree[["children"]])) {
        ## let's compute the local model and return it
        ## prunable is true as there is no sub tree
        if (verbose) {
          print(paste(ctx, collapse = " "))
          print(paste("call to glm with d=", d, sep = ""))
        }
        res <- list(
          model = node_fit_glm(tree$match, d, y, covariate, alpha, nb_vals),
          match = tree$match,
          f_by = tree$f_by
        )
        res$prunable <- TRUE
        if (verbose) {
          print(res$model$hsize)
        }
        res
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
              recurse_ctx_tree_fit_glm(tree$children[[v]], c(ctx, v), d + 1, y, covariate)
            if (!is.null(submodels[[v]][["model"]])) {
              nb_models <- nb_models + 1
              prunable <- submodels[[v]][["prunable"]]
              if (isTRUE(prunable)) {
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

        result <- list(f_by = tree$f_by, match = tree$match)
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
            print(paste("fitting a local model (of full rank) with", d, "covariates"))
          }
          local_model <- node_fit_glm(tree$match, d, y, covariate, alpha, nb_vals, return_all = TRUE)
          if (verbose) {
            print(paste(local_model$H1_model$H0, local_model$H1_model$hsize))
          }
        }
        if (need_merged_model) {
          ## we need a merged model
          if (verbose) {
            print(paste("fitting a merged model for:", paste(pr_candidates, collapse = " ")))
          }
          ## prepare the data set
          ## we need to reextract the data as models can use different history sizes
          ## shift the index by one to account for the reduce history
          full_index <- 1 + unlist(lapply(submodels[pr_candidates], function(x) x$match))
          if (verbose) {
            print(paste("call to glm with d=", d, sep = ""))
          }
          local_model <- node_fit_glm(full_index, d, y, covariate, alpha, nb_vals, return_all = TRUE)
        }
        if (!is.null(local_model)) {
          ## to compute the likelihood of the new model on the data used by
          ## the ones to merge/remove we need to reextract the data if the models
          ## have a shorter history than the one used be the local model
          ll_model_H0 <- 0
          ll_H0 <- 0
          local_df <- (1 + ncol(covariate) * local_model$H1_model$hsize) * (nb_vals - 1)
          sub_df <- 0
          for (v in pr_candidates) {
            sub_df <- sub_df + (1 + ncol(covariate) * submodels[[v]][["model"]]$hsize) * (nb_vals - 1)
            if (verbose) {
              print(paste(v, submodels[[v]][["model"]]$hsize, local_model$H1_model$hsize))
            }
            if (submodels[[v]][["model"]]$hsize == local_model$H1_model$hsize) {
              local_data <- submodels[[v]][["model"]]$data
              mm <- submodels[[v]][["model"]]$data$local_mm
              target <- submodels[[v]][["model"]]$data$target
            } else {
              local_data <- prepare_glm(covariate, submodels[[v]]$match, d, y)
              if (verbose) {
                print("preparing local data")
                # print(head(local_data$local_mm))
              }
            }
            ll_model_H0 <- ll_model_H0 +
              glm_likelihood(
                local_model$H1_model$model,
                local_data$local_mm,
                local_data$target
              )
            ll_H0 <- ll_H0 + submodels[[v]][["model"]]$likelihood
          }
          if (verbose) {
            print(paste("# of parameters", local_df, sub_df))
          }
          lambda <- 2 * (ll_H0 - ll_model_H0)
          p_value <- stats::pchisq(as.numeric(lambda),
            df = sub_df - local_df,
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
            if (is.na(local_model$p_value)) {
              result$model <- local_model$H0_model
            } else if (local_model$p_value > alpha) {
              result$model <- local_model$H0_model
            } else {
              result$model <- local_model$H1_model
            }
            result$prunable <- TRUE
            if (verbose) {
              print(result$model$model)
            }
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
            if (is.na(local_model$p_value)) {
              result$merged_model <- local_model$H0_model
            } else if (local_model$p_value > alpha) {
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
              print(paste("Trying to prune covariables", paste(pr_candidates, collapse = " ")))
            }
            for (v in pr_candidates) {
              result$children[[v]][["model"]] <-
                node_prune_model(result$children[[v]][["model"]], ncol(covariate), nb_vals, alpha, keep_data)
            }
          }
        } else {
          if (result$merged_model$H0) {
            if (verbose) {
              print("Trying to prune the merged model covariables")
            }
            result$merged_model <- node_prune_model(result$merged_model, ncol(covariate), nb_vals, alpha, keep_data)
          }
        }
        result
      }
    }
  result <- recurse_ctx_tree_fit_glm(tree, c(), 0, y, covariate)
  result$vals <- tree$vals
  result
}

count_covlmc_local_context <- function(node) {
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
  if (length(vals) > 2) {
    x <- nx$fx
  }
  ctx_tree$match <- 1:length(x)
  pruned_tree <- ctx_tree_fit_glm(ctx_tree, x, covariate,
    alpha = alpha, verbose = FALSE, keep_data = TRUE
  )
  pre_result <- new_ctx_tree(pruned_tree$vals, pruned_tree, count_context = count_covlmc_local_context, class = "covlmc")
  pre_result$cov_names <- names(covariate)
  pre_result
}

#' @export
context_number.covlmc <- function(ct) {
  if (!is.null(ct$nb_ctx)) {
    ct$nb_ctx
  } else {
    rec_context_number(ct, count_covlmc_local_context)
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
