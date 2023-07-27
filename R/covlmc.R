## fit a glm guaranteed to be of full rank by removing older covariates if needed
node_fit_glm_full_rank <- function(index, y, covariate, nb_vals, d, control, from = 0) {
  glmdata <- prepare_glm(covariate, index, d, y, from)
  node_fit_glm_full_rank_with_data(glmdata$local_mm, d, glmdata$target, ncol(covariate), nb_vals, control)
}

## implementation of node_fit_glm_full_rank
node_fit_glm_full_rank_with_data <- function(local_mm, d, target, dim_cov, nb_vals, control) {
  if (nrow(local_mm) > 0) {
    local_glm <- fit_glm(target, local_mm, nb_vals, control)
    while (is_glm_low_rank(local_glm)) {
      d <- d - 1L
      local_mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = dim_cov), drop = FALSE]
      local_glm <- fit_glm(target, local_mm, nb_vals, control)
    }
    list(
      coefficients = glm_coef(local_glm, local_mm),
      var_names = glm_variable_names(local_glm, local_mm),
      likelihood = as.numeric(stats::logLik(local_glm)),
      data = list(local_mm = local_mm, target = target),
      model = local_glm,
      hsize = d,
      metrics = glm_metrics(local_glm, local_mm, target)
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
node_fit_glm <- function(index, d, y, covariate, alpha, nb_vals, return_all = FALSE, control, from = 0) {
  ## prepare the data
  glmdata <- prepare_glm(covariate, index, d, y, from)
  node_fit_glm_with_data(glmdata$local_mm, d, glmdata$target, ncol(covariate), alpha, nb_vals, return_all, control)
}

## implementation of node_fit_glm
node_fit_glm_with_data <- function(local_mm, d, target, dim_cov, alpha, nb_vals, return_all = FALSE, control) {
  ## compute the full rank "H1" model
  full_rank_model <- node_fit_glm_full_rank_with_data(
    local_mm,
    d,
    target,
    dim_cov, nb_vals, control
  )
  if (!is.null(full_rank_model)) {
    if (full_rank_model$hsize < d || d < 1) {
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
        d - 1L, target,
        dim_cov, nb_vals, control
      )
      full_rank_model$H0 <- FALSE
      H0_full_rank_model$H0 <- TRUE
      lambda <- 2 * (full_rank_model$likelihood - H0_full_rank_model$likelihood)
      df <- (full_rank_model$hsize - H0_full_rank_model$hsize) * dim_cov * (nb_vals - 1L)
      p_value <-
        stats::pchisq(as.numeric(lambda), df = df, lower.tail = FALSE)
      if (is.na(p_value)) {
        print(paste(p_value, lambda))
      }
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

node_prune_model <- function(model, cov_dim, nb_vals, alpha, keep_data = FALSE, control, verbose = FALSE) {
  local_mm <- model$data$local_mm
  target <- model$data$target
  if (ncol(local_mm) >= 1) {
    nb <- ncol(local_mm) %/% cov_dim
    current_like <- model$likelihood
    previous_model <- model$model
    current_model <- NULL
    current_data <- NULL
    p_value <- NA
    hsize <- model$hsize
    for (k in 1:nb) {
      if (verbose) {
        print(paste("node_prune_model", k))
      }
      h0mm <- local_mm[, -seq(ncol(local_mm), by = -1, length.out = cov_dim * k), drop = FALSE]
      H0_local_glm <- fit_glm(target, h0mm, nb_vals, control)
      if (is_glm_low_rank(H0_local_glm)) {
        warning("pruned model is rank deficient will non pruned model is not")
      }
      lambda <- 2 * (current_like - stats::logLik(H0_local_glm))
      p_value <- stats::pchisq(as.numeric(lambda), df = cov_dim * (nb_vals - 1), lower.tail = FALSE)
      if (is.na(p_value)) {
        print(paste(p_value, lambda))
      }
      if (p_value > alpha) {
        ## H0 is not rejected
        current_like <- as.numeric(stats::logLik(H0_local_glm))
        current_model <- H0_local_glm
        previous_model <- H0_local_glm
        if (keep_data) {
          current_data <- list(local_mm = h0mm, target = target)
        }
        hsize <- hsize - 1L
      } else {
        ## H0 is rejected, we break the loop
        if (verbose) {
          print("rejecting H0")
        }
        ## we need to propagate the p-value
        if (is.null(current_model)) {
          model$p_value <- p_value
        }
        break
      }
    }
    if (is.null(current_model)) {
      ## we keep the original model
      if (!keep_data) {
        model$data <- NULL
      }
      ## make sure we have a p_value
      ## should not happen
      if (is.null(model$p_value) || is.na(model$p_value)) {
        warning("Model with no p_value in node_prune_model")
        model$p_value <- p_value
      }
      model
    } else {
      list(
        H0 = FALSE, ## is this the correct return value?
        coefficients = glm_coef(current_model, local_mm),
        var_names = glm_variable_names(current_model, local_mm),
        likelihood = current_like,
        data = current_data,
        model = current_model,
        hsize = hsize,
        metrics = glm_metrics(current_model, local_mm, target),
        p_value = p_value
      )
    }
  } else {
    if (!keep_data) {
      model$data <- NULL
    }
    model
  }
}

ctx_tree_fit_glm <- function(tree, y, covariate, alpha, control, assume_model = FALSE, keep_local_data = FALSE, keep_model = FALSE,
                             verbose = FALSE) {
  nb_vals <- length(tree$vals)
  recurse_ctx_tree_fit_glm <-
    function(tree, ctx, d, y, covariate) {
      if (length(tree) == 0) {
        ## dead end marker, nothing to do (should not happen)
        list()
      } else if (is.null(tree[["children"]])) {
        if (assume_model) {
          if (!is.null(tree[["model"]])) {
            if (tree$model$hsize < d || is.na(tree$model$p_value) || tree$model$p_value <= alpha) {
              tree$prunable <- TRUE
              return(tree)
            }
          } else if (nb_vals == 2) {
            stop("internal error in ctx_free_fit_glm: missing model with assume_model = TRUE")
          }
          if (verbose) {
            print("model recomputation is needed")
          }
        }
        ## let's compute the local model and return it
        ## prunable is true as there is no sub tree
        if (verbose) {
          print(paste(ctx, collapse = " "))
          print(paste("call to glm with d=", d, sep = ""))
        }
        res <- list(
          model = node_fit_glm(tree$match, d, y, covariate, alpha, nb_vals, control = control),
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
        nb_models <- 0L
        nb_children <- 0L
        nb_rejected <- 0L
        nb_prunable <- 0L
        max_hsize <- 0L
        pr_candidates <- c()
        ll_H0 <- 0
        for (v in seq_along(tree$children)) {
          if (ctx_tree_exists(tree$children[[v]])) {
            nb_children <- nb_children + 1L
            submodels[[v]] <-
              recurse_ctx_tree_fit_glm(tree$children[[v]], c(ctx, v), d + 1L, y, covariate)
            if (!is.null(submodels[[v]][["model"]])) {
              nb_models <- nb_models + 1L
              prunable <- submodels[[v]][["prunable"]]
              if (isTRUE(prunable)) {
                if (assume_model) {
                  if (submodels[[v]][["model"]]$hsize == d + 1L) {
                    nb_rejected <- nb_rejected + 1L
                  } else {
                    pr_candidates <- c(pr_candidates, v)
                    nb_prunable <- nb_prunable + 1L
                    ll_H0 <-
                      ll_H0 + submodels[[v]][["model"]]$likelihood
                    max_hsize <- max(max_hsize, submodels[[v]][["model"]]$hsize)
                  }
                } else {
                  if (!submodels[[v]][["model"]]$H0) {
                    nb_rejected <- nb_rejected + 1L
                  } else {
                    pr_candidates <- c(pr_candidates, v)
                    nb_prunable <- nb_prunable + 1L
                    ll_H0 <-
                      ll_H0 + submodels[[v]][["model"]]$likelihood
                    max_hsize <- max(max_hsize, submodels[[v]][["model"]]$hsize)
                  }
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
        p_value <- NULL
        if (need_local_model) {
          ## we need a local model
          if (!is.null(tree[["cache"]]) && !is.null(tree[["cache"]][["model"]])) {
            local_model <- tree[["cache"]][["model"]]
            p_value <- tree[["cache"]][["p_value"]]
            if (verbose) {
              print("Reusing cached model")
            }
          } else {
            if (verbose) {
              print(paste("fitting a local model (of full rank) with hsize", max_hsize, "at depth", d))
            }
            local_model <- node_fit_glm(tree$match, max_hsize, y, covariate, alpha, nb_vals, return_all = TRUE, control, d - max_hsize)
            if (verbose) {
              print(paste(local_model$H1_model$H0, local_model$H1_model$hsize))
            }
          }
        }
        if (need_merged_model) {
          ## we need a merged model
          if (!is.null(tree[["cache"]]) && !is.null(tree[["cache"]][["merged_model"]])) {
            local_model <- tree[["cache"]][["merged_model"]]
            p_value <- tree[["cache"]][["p_value"]]
            if (verbose) {
              print("Reusing cached model")
            }
          } else {
            if (verbose) {
              print(paste("fitting a merged model for:", paste(pr_candidates, collapse = " ")))
            }
            ## prepare the data set
            ## we need to reextract the data as models can use different history sizes
            ## shift the index by one to account for the reduced history
            full_index <- 1L + unlist(lapply(submodels[pr_candidates], function(x) x$match))
            if (verbose) {
              print(paste("call to glm with d=", d, sep = ""))
            }
            local_model <- node_fit_glm(full_index, max_hsize, y, covariate, alpha, nb_vals, return_all = TRUE, control, d - max_hsize)
          }
        }
        if (!is.null(local_model) && is.null(p_value)) {
          ## to compute the likelihood of the new model on the data used by
          ## the ones to merge/remove we need to reextract the data if the models
          ## have a shorter history than the one used by the local model
          ll_model_H0 <- 0
          ll_H0 <- 0
          ## the local_modal can be either H1 or H0
          if (is.na(local_model$p_value)) {
            if (local_model$H0_model$H0) {
              actual_model <- local_model$H0_model
            } else {
              actual_model <- local_model$H1_model
            }
          } else {
            if (local_model$p_value > alpha) {
              actual_model <- local_model$H0_model
            } else {
              actual_model <- local_model$H1_model
            }
          }
          local_df <- (1L + ncol(covariate) * actual_model$hsize) * (nb_vals - 1L)
          sub_df <- 0L
          for (v in pr_candidates) {
            sub_df <- sub_df + (1L + ncol(covariate) * submodels[[v]][["model"]]$hsize) * (nb_vals - 1L)
            if (verbose) {
              print(paste(v, submodels[[v]][["model"]]$hsize, actual_model$hsize))
            }
            if (submodels[[v]][["model"]]$hsize == actual_model$hsize) {
              local_data <- submodels[[v]][["model"]]$data
            } else {
              local_data <- prepare_glm(covariate, 1L + submodels[[v]]$match, max_hsize, y, d - max_hsize)
              if (verbose) {
                print("preparing local data")
                print(paste(ctx, collapse = ", "))
                for (tmp in 1:5) {
                  print(y[(tree$match[tmp] + 1L):(tree$match[tmp] + d + 1L)])
                  print(y[(submodels[[v]]$match[tmp] + 1L):(submodels[[v]]$match[tmp] + d + 1L)])
                  print("")
                }
              }
            }
            ll_model_H0_sub <- glm_likelihood(actual_model$model, local_data$local_mm, local_data$target)
            if (is.na(ll_model_H0_sub)) {
              print(utils::head(local_data$local_mm))
              print(utils::head(local_data$local_mm))
            }
            ll_model_H0 <- ll_model_H0 + ll_model_H0_sub
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
          if (is.na(p_value)) {
            print(paste(ll_H0, ll_model_H0, lambda, sub_df, local_df, max_hsize, d))
            print(local_model$H1_model$model)
            for (v in pr_candidates) {
              print(submodels[[v]]$model$model)
            }
          }
          if (verbose) {
            print(paste(lambda, p_value))
          }
        }
        ## let prepare first the children to keep
        if (need_local_model) {
          if (!is.na(p_value) && p_value > alpha) {
            ## we remove the prunable subtrees all together
            if (verbose) {
              print("pruning the subtree")
            }
            if (is.na(local_model$p_value)) {
              result$model <- local_model$H0_model
            } else {
              if (local_model$p_value > alpha) {
                result$model <- local_model$H0_model
              } else {
                result$model <- local_model$H1_model
              }
              result$model$p_value <- local_model$p_value
            }
            result$prunable <- TRUE
            if (verbose) {
              print(result$model$model)
            }
          } else {
            ## we throw away the local model and keep the children
            result$children <- submodels
            result$prunable <- FALSE
            result$p_value <- p_value
            if (keep_model) {
              result$cache <- list(model = local_model, p_value = p_value)
            }
          }
        } else if (need_merged_model) {
          result$prunable <- FALSE
          if (!is.na(p_value) && p_value > alpha) {
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
            } else {
              if (local_model$p_value > alpha) {
                result$merged_model <- local_model$H0_model
              } else {
                result$merged_model <- local_model$H1_model
              }
              result$merged_model$p_value <- local_model$p_value
            }
            result$merged <- pr_candidates
          } else {
            ## we throw away the local model and keep the children
            result$merged_p_value <- p_value
            result$merged_candidates <- pr_candidates
            result$children <- submodels
            if (keep_model) {
              result$cache <- list(merged_model = local_model)
            }
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
                node_prune_model(result$children[[v]][["model"]], ncol(covariate), nb_vals, alpha, keep_local_data, control)
            }
          }
        } else {
          if (result$merged_model$H0) {
            if (verbose) {
              print("Trying to prune the merged model covariables")
            }
            result$merged_model <- node_prune_model(result$merged_model, ncol(covariate), nb_vals, alpha, keep_local_data, control)
          }
        }
        result
      }
    }
  result <- recurse_ctx_tree_fit_glm(tree, c(), 0L, y, covariate)
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

#' Control for coVLMC fitting
#'
#' This function creates a list with parameters used to fine tune the coVLMC
#' fitting algorithm.
#'
#' `pseudo_obs` is used to regularize the probability estimations when a
#' context is only observed followed by always the same state. Transition
#' probabilities are computed after adding `pseudo_obs` pseudo observations
#' of each of the states (including the observed one). This corresponds to a
#' Bayesian posterior mean estimation with a Dirichlet prior.
#'
#' @param pseudo_obs number of fake observations of each state to add to the
#'   observed ones.
#'
#' @returns a list.
#' @export
#' @examples
#' dts <- rep(c(0, 1), 100)
#' dts_cov <- data.frame(y = rep(0, length(dts)))
#' default_model <- covlmc(dts, dts_cov)
#' contexts(default_model, type = "data.frame", model = "coef")$coef
#' control <- covlmc_control(pseudo_obs = 10)
#' model <- covlmc(dts, dts_cov, control = control)
#' contexts(model, type = "data.frame", model = "coef")$coef
covlmc_control <- function(pseudo_obs = 1) {
  list(pseudo_obs = pseudo_obs)
}

#' Fit a Variable Length Markov Chain with Covariates (coVLMC)
#'
#' This function fits a  Variable Length Markov Chain with covariates (coVLMC)
#' to a discrete time series coupled with a time series of covariates.
#'
#' @param x a discrete time series; can be numeric, character, factor or logical.
#' @param covariate a data frame of covariates.
#' @param alpha number in (0,1) (default: 0.05) cut off value in the pruning
#'   phase (in quantile scale).
#' @param min_size number >= 1 (default: 5). Tune the minimum number of
#'   observations for a context in the growing phase of the context tree (see
#'   below for details).
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'   growing phase of the context tree.
#' @param keep_data logical (defaults to `TRUE`). If `TRUE`, the original data
#'   are stored in the resulting object to enable post pruning (see
#'   [prune.covlmc()]).
#' @param control a list with control parameters, see [covlmc_control()].
#' @param ... arguments passed to [covlmc_control()].
#' @returns a fitted covlmc model.
#'
#' @details
#'
#' The model is built using the algorithm described in Zanin Zambom et al. As
#' for the [vlmc()] approach, the algorithm builds first a context tree (see
#' [ctx_tree()]). The `min_size` parameter is used to compute the actual number
#' of observations per context in the growing phase of the tree. It is computed
#' as `min_size*(1+ncol(covariate)*d)*(s-1)` where `d` is the length of the
#' context (a.k.a. the depth in the tree) and `s` is the number of states. This
#' corresponds to ensuring min_size observations per parameter of the logistic
#' regression during the estimation phase.
#'
#' Then logistic models are adjusted in the leaves at the tree: the goal of each
#' logistic model is to estimate the conditional distribution of the next state
#' of the times series given the context (the recent past of the time series)
#' and delayed versions of the covariates. A pruning strategy is used to
#' simplified the models (mainly to reduce the time window associated to the
#' covariates) and the tree itself.
#'
#' Parameters specified by `control` are used to fine tune the behaviour of the
#' algorithm.
#'
#' @section Logistic models:
#'
#'   By default, `covlmc` uses two different computing _engines_ for logistic
#'   models:
#'
#'   - when the time series has only two states, `covlmc` uses [stats::glm()]
#'   with a binomial link ([stats::binomial()]);
#'   - when the time series has at least three
#'   states, `covlmc` use [VGAM::vglm()] with a multinomial link
#'   ([VGAM::multinomial()]).
#'
#'   Both engines are able to detect degenerate cases and lead to more robust
#'   results that using [nnet::multinom()]. It is nevertheless possible to
#'   replace [stats::glm()] and [VGAM::vglm()] with [nnet::multinom()] by setting
#'   the global option `mixvlmc.predictive` to `"multinom"` (the default value is
#'   `"glm"`). Notice that while results should be comparable, there is no
#'   guarantee that they will be identical.
#'
#' @references
#'
#' - Bühlmann, P. and Wyner, A. J. (1999), "Variable length Markov chains." Ann.
#' Statist. 27 (2) 480-513 \doi{10.1214/aos/1018031204}
#' - Zanin Zambom, A., Kim, S. and Lopes Garcia, N. (2022), "Variable length Markov chain
#' with exogenous covariates." J. Time Ser. Anal., 43 (2)
#' 312-328 \doi{10.1111/jtsa.12615}
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(1 / 3, 2 / 3, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 15)
#' draw(m_cov)
#' withr::with_options(
#'   list(mixvlmc.predictive = "multinom"),
#'   m_cov_nnet <- covlmc(dts, dts_cov, min_size = 15)
#' )
#' draw(m_cov_nnet)
#' @seealso [cutoff.covlmc()] and [prune.covlmc()] for post-pruning.
covlmc <- function(x, covariate, alpha = 0.05, min_size = 5L, max_depth = 100L, keep_data = TRUE, control = covlmc_control(...), ...) {
  assertthat::assert_that(is.data.frame(covariate))
  assertthat::assert_that(nrow(covariate) == length(x))
  # data conversion
  nx <- to_dts(x)
  ix <- nx$ix
  vals <- nx$vals
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  ## covariate preparation
  desc <- covariate_description(covariate)
  cov_desc <- desc$cov_desc
  covariate <- desc$covariate
  ## we enforce a full context tree (with all_children=TRUE)
  ## min_size is multiplied by the state space cardinal minus 1 as the semantics
  ## of grow_ctx_tree is to multiply min_size by 1+depth*covsize in order to
  ## work without modification or test is covsize==0
  ctx_tree <- grow_ctx_tree(ix, vals,
    min_size = min_size * (length(vals) - 1L), max_depth = max_depth,
    covsize = desc$cov_size, keep_match = TRUE, all_children = TRUE
  )
  if (length(vals) > 2) {
    x <- nx$fx
  } else {
    x <- ix
  }
  ctx_tree$match <- 1:length(x)
  pruned_tree <- ctx_tree_fit_glm(ctx_tree, x, covariate,
    alpha = alpha, control = control, assume_model = FALSE,
    keep_local_data = TRUE, keep_model = keep_data, verbose = FALSE
  )
  pre_result <- new_ctx_tree(pruned_tree$vals, pruned_tree, count_context = count_covlmc_local_context, class = "covlmc")
  pre_result$cov_names <- names(covariate)
  pre_result$alpha <- alpha
  pre_result$control <- control
  pre_result$cov_desc <- cov_desc
  pre_result$max_depth <- ctx_tree$max_depth
  if (keep_data) {
    pre_result$x <- x
    pre_result$covariate <- covariate
  }
  pre_result
}

#' Test if the object is a covlmc model
#'
#' This function returns `TRUE` for VLMC models with covariates and `FALSE` for other objects.
#'
#' @param x an R object.
#' @returns `TRUE` for VLMC models with covariates.
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' # should be true
#' is_ctx_tree(m_cov)
#' # should be true
#' is_covlmc(m_cov)
#' # should be false
#' is_vlmc(m_cov)
is_covlmc <- function(x) {
  inherits(x, "covlmc")
}

assertthat::on_failure(is_covlmc) <- function(call, env) {
  paste0(deparse(call$x), " is not a covlmc")
}


#' Cutoff values for pruning the context tree of a VLMC with covariates
#'
#' This function returns all the cutoff values that should induce a pruning of
#' the context tree of a VLMC with covariates.
#'
#' Notice that the list of cutoff values returned by the function is not as
#' complete as the one computed for a VLMC without covariates. Indeed, pruning
#' the coVLMC tree creates new pruning opportunities that are not evaluated
#' during the construction of the initial model, while all pruning opportunities
#' are computed during the construction of a VLMC context tree. Nevertheless,
#' the largest value returned by the function is guaranteed to produce the least
#' pruned tree consistent with the reference one.
#'
#' Notice that the loglikelihood scale is not directly useful in coVLMC as
#' the differences in model sizes are not constant through the pruning process.
#' As a consequence, the "native" scale is not supported by this function.
#'
#' Setting `raw` to `TRUE` removes the small perturbation that are subtracted from
#'  the log-likelihood ratio values computed from the coVLMC (in quantile scale).
#'
#' @param vlmc a fitted covlmc model.
#' @param mode specify whether the results should be "native" likelihood ratio
#'   values or expressed in a "quantile" scale of a chi-squared distribution.
#'   For covlmc, only the quantile scale is supported.
#' @param raw specify whether the returned values should be limit values computed in the model or
#'  modified values that guarantee pruning (see details)
#' @param ... additional arguments for the cutoff function.
#' @returns a vector of cut off values, `NULL` is none can be computed
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' m_nocovariate <- vlmc(dts)
#' draw(m_nocovariate)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' draw(m_cov)
#' cutoff(m_cov)
#' @export
cutoff.covlmc <- function(vlmc, mode = c("quantile", "native"), raw = FALSE, ...) {
  mode <- match.arg(mode)
  if (mode == "native") {
    stop("native mode is not supported by covlmc objects")
  }
  recurse_cutoff <- function(tree) {
    if (is.null(tree[["children"]])) {
      if (is.null(tree[["model"]])) {
        # nothing there (should not happen)
        NULL
      } else {
        p_value <- NULL
        if (!is.null(tree$model[["p_value"]])) {
          p_value <- tree$model[["p_value"]]
          if (is.na(p_value) || (length(tree$model[["coefficients"]]) == 1 && p_value > vlmc$alpha)) {
            p_value <- NULL
          }
        }
        p_value
      }
    } else {
      df <- NULL
      for (v in seq_along(tree[["children"]])) {
        sub_p <- recurse_cutoff(tree$children[[v]])
        df <- c(df, sub_p)
      }
      if (!is.null(tree[["merged_model"]])) {
        if (!is.na(tree[["merged_model"]]$p_value)) {
          df <- c(df, tree[["merged_model"]]$p_value)
        }
      }
      c(df, tree$p_value, tree$merged_p_value)
    }
  }
  preres <- recurse_cutoff(vlmc)
  if (is.null(preres)) {
    NULL
  } else {
    preres <- unique(sort(preres, decreasing = TRUE))
    if (!raw) {
      preres <- before(preres)
      preres[preres < 0] <- 0
    }
    if (length(preres) == 1 && preres[1] == 0) {
      NULL
    } else {
      preres
    }
  }
}

#' Prune a Variable Length Markov Chain with covariates
#'
#' This function prunes a vlmc with covariates. This model must have been
#' estimated with `keep_data=TRUE` to enable the pruning.
#'
#' Post pruning a VLMC with covariates is not as straightforward as the same
#' procedure applied to [vlmc()] (see [cutoff.vlmc()] and [prune.vlmc()]). For
#' efficiency reasons, [covlmc()] estimates only the logistic models that are
#' considered useful for a given set construction parameters. With a more
#' aggressive pruning threshold, some contexts become leaves of the context tree
#' and new logistic models must be estimated. Thus the pruning opportunities
#' given by [cutoff.covlmc()] are only a subset of interesting cut offs for a
#' given covlmc.
#'
#' Nevertheless, `covlmc` share with [vlmc()] the principle that post pruning a
#' covlmc should give the same model as buidling directly the covlmc, provided
#' that the post pruning alpha is smaller than the alpha used to build the
#' initial model.
#'
#' @param vlmc a fitted VLMC model with covariates.
#' @param alpha number in (0,1) (default: 0.05) cutoff value in quantile scale
#'   for pruning.
#' @param cutoff not supported by the vlmc with covariates.
#' @param ... additional arguments for the prune function.
#'
#' @returns a pruned covlmc.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5, keep_data = TRUE)
#' draw(m_cov)
#' m_cov_cuts <- cutoff(m_cov)
#' p_cov <- prune(m_cov, m_cov_cuts[1])
#' draw(p_cov)
#' @export
prune.covlmc <- function(vlmc, alpha = 0.05, cutoff = NULL, ...) {
  if (is.null(vlmc$x) || is.null(vlmc$covariate)) {
    stop("covlmc must be called with keep_data=TRUE to enable post pruning")
  }
  if (!is.null(cutoff)) {
    stop("covlmc does not support cutoff based pruning")
  }
  if (is.null(alpha) || !is.numeric(alpha) || alpha <= 0) {
    stop("the alpha parameter must be a strictly positive number")
  }
  pruned_tree <- ctx_tree_fit_glm(vlmc, vlmc$x, vlmc$covariate,
    alpha = alpha, control = vlmc$control, assume_model = TRUE,
    keep_local_data = TRUE, keep_model = TRUE, verbose = FALSE
  )
  pre_result <- new_ctx_tree(vlmc$vals, pruned_tree, count_context = count_covlmc_local_context, class = "covlmc")
  pre_result$cov_names <- vlmc$cov_names
  pre_result$cov_desc <- vlmc$cov_desc
  pre_result$alpha <- alpha
  pre_result$x <- vlmc$x
  pre_result$covariate <- vlmc$covariate
  pre_result$control <- vlmc$control
  pre_result
}

#' @export
print.covlmc <- function(x, ...) {
  cat(paste(
    "VLMC with covariate context tree on",
    paste(x$vals, collapse = ", ")
  ), "\n")
  cat(paste(" cutoff in quantile scale: ", signif(x$alpha, 4), "\n", sep = ""))
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}
