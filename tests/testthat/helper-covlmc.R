build_data_set <- function(n, seed = 0) {
  withr::local_seed(seed)
  blink <- stats::binomial()
  covariate <- data.frame(y = runif(n))
  coeffs <- matrix(c(-1, 2, 1, -2), ncol = 2)
  scores <- cbind(1, covariate$y) %*% coeffs
  probs <- apply(scores, 1, blink$linkinv)
  x <- rep(0, n)
  x[1] <- sample(c(0, 1), 1)
  for (k in 2:n) {
    prob <- probs[x[k - 1] + 1, k - 1]
    x[k] <- sample(c(0, 1), 1, prob = c(1 - prob, prob))
  }
  list(x = x, covariate = covariate)
}

build_data_set_2 <- function(seed = 0) {
  withr::local_seed(seed)
  y <- as.factor(rep(rep(1:4, each = 50), 2))
  z <- runif(length(y))
  rdts_cov <- data.frame(y = y, z = z)
  cov_dep <- matrix(runif(length(levels(y)) * 4, min = -1, max = 1), nrow = length(levels(y)))
  cov_dep[cov_dep > 0.5] <- 1
  cov_dep[cov_dep < -0.5] <- -1
  cov_dep[cov_dep <= 0.5 & cov_dep >= -0.5] <- 0
  x <- rep(0, length(y))
  x[2] <- sample(c(0, 1), 1)
  probs <- rep(NA, length(y))
  for (k in 3:length(y)) {
    probs[k] <- 1 / (1 + exp(-sum(cov_dep[as.integer(y[k - 1]), ] * c(1, x[k - 1], z[c(k - 1, k - 2)]))))
    x[k] <- ifelse(runif(1) <= probs[k], 1, 0)
  }
  x <- as.factor(x)
  list(x = x, covariate = rdts_cov)
}

build_data_set_3_model <- function(n, seed = 0, alpha = 0.1) {
  withr::local_seed(seed)
  x3 <- sample(c("A", "B", "C"), n, replace = TRUE)
  y3 <- ifelse(runif(length(x3)) > 0.5, c(x3[-1], sample(c("A", "B", "C"), 1)), c(x3[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y3 <- as.factor(ifelse(runif(length(x3)) > 0.2, y3, sample(c("A", "B", "C"), n, replace = TRUE)))
  z3 <- data.table::fcase(
    x3[-1] == "A", runif(length(x3) - 1),
    x3[-1] == "B", 0.5 + runif(length(x3) - 1),
    x3[-1] == "C", -0.5 + runif(length(x3) - 1)
  )
  z3 <- c(0, z3)
  df_full <- data.frame(y = y3, z = z3)
  model <- covlmc(x3, df_full, max_depth = 5, min_size = 5, alpha = alpha)
  list(model = model, rdts = x3, cov = df_full)
}

extract_p_value <- function(tree) {
  recurse_extract_p_value <- function(tree) {
    if (is.null(tree[["children"]])) {
      if (is.null(tree[["model"]])) {
        # nothing there (should not happen)
        NULL
      } else {
        p_value <- NA
        if (!is.null(tree$model[["p_value"]])) {
          p_value <- tree$model[["p_value"]]
        }
        data.frame(p_value = p_value, nb_coeffs = length(tree$model[["coefficients"]]))
      }
    } else {
      df <- NULL
      for (v in seq_along(tree[["children"]])) {
        sub_p <- recurse_extract_p_value(tree$children[[v]])
        if (!is.null(sub_p)) {
          df <- rbind(df, sub_p)
        }
      }
      if (!is.null(tree[["merged_model"]])) {
        if (!is.na(tree[["merged_model"]]$p_value)) {
          sub_p <- data.frame(
            p_value = tree[["merged_model"]]$p_value,
            nb_coeffs = length(tree[["merged_model"]]$coefficients)
          )
          df <- rbind(df, sub_p)
        }
      }
      if (!is.null(tree$p_value)) {
        sub_p <- data.frame(
          p_value = tree$p_value,
          nb_coeffs = NA
        )
        df <- rbind(df, sub_p)
      }
      if (!is.null(tree$merged_p_value)) {
        sub_p <- data.frame(
          p_value = tree$merged_p_value,
          nb_coeffs = NA
        )
        df <- rbind(df, sub_p)
      }
      df
    }
  }
  recurse_extract_p_value(tree)
}

compare_covlmc_node <- function(n1, n2) {
  for (what in c("f_by", "match", "prunable", "vals", "depth", "nb_ctx", "cov_names", "alpha", "merged")) {
    if (!identical(n1[[what]], n2[[what]])) {
      return(FALSE)
    }
  }
  if (!is.null(n1[["model"]])) {
    if (is.null(n2[["model"]])) {
      return(FALSE)
    }
    if (!identical(n1[["model"]][["coefficents"]], n2[["model"]][["coefficents"]])) {
      return(FALSE)
    }
  }
  if (!is.null(n1[["merged_model"]])) {
    if (is.null(n2[["merged_model"]])) {
      return(FALSE)
    }
    if (!identical(n1[["merged_model"]][["coefficents"]], n2[["merged_model"]][["coefficents"]])) {
      return(FALSE)
    }
  }
  if (!is.null(n1[["extended_model"]])) {
    if (is.null(n2[["extended_model"]])) {
      return(FALSE)
    }
    if (!identical(n1[["extended_model"]][["coefficents"]], n2[["extended_model"]][["coefficents"]])) {
      return(FALSE)
    }
  }
  TRUE
}

compare_covlmc <- function(m1, m2) {
  rec_compare_covlmc <- function(m1, m2) {
    if (!compare_covlmc_node(m1, m2)) {
      return(FALSE)
    }
    if (is.null(m1[["children"]])) {
      if (!is.null(m2[["children"]])) {
        return(FALSE)
      }
      return(TRUE)
    } else {
      if (is.null(m2[["children"]])) {
        return(FALSE)
      }
      if (length(m1[["children"]]) != length(m2[["children"]])) {
        return(FALSE)
      }
      for (v in seq_along(m1[["children"]])) {
        if (!rec_compare_covlmc(m1[["children"]][[v]], m2[["children"]][[v]])) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
  }
  rec_compare_covlmc(m1, m2)
}

build_degenerate_elec_model <- function(with_new_data = FALSE) {
  pc_week_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
  elec <- pc_week_15_16$active_power
  elec_rdts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
  elec_tune <- tune_covlmc(elec_rdts, elec_cov, min_size = 5)
  elec_model <- prune(as_covlmc(elec_tune), alpha = 3.961e-10)
  result <- list(model = elec_model, rdts = elec_rdts, cov = elec_cov)
  if (with_new_data) {
    pc_week_17_18 <- powerconsumption[powerconsumption$week %in% c(17, 18), ]
    elec_new_rdts <- cut(pc_week_17_18$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
    elec_new_cov <- data.frame(day = (pc_week_17_18$hour >= 7 & pc_week_17_18$hour <= 18))
    result$new_cov <- elec_new_cov
    result$new_rdts <- elec_new_rdts
  }
  result
}

## same calculations as in slow_loglikelihood but for COVLMC
co_slow_loglikelihood <- function(model, newdata, initial = c("truncated", "specific", "extended"),
                                  ignore, newcov, verbose = FALSE) {
  initial <- match.arg(initial)
  if (missing(ignore)) {
    if (initial == "truncated") {
      ignore <- depth(model)
    } else {
      ignore <- 0L
    }
  } else if (initial == "truncated" && ignore < depth(model)) {
    stop("Must ignore at least ", depth(model), " for truncated likelihood")
  }
  nx <- to_dts(newdata, model$vals)
  if (length(model$vals) > 2) {
    newdata <- nx$fx
  } else {
    newdata <- nx$ix
  }
  x <- nx$ix + 1
  ctx <- c()
  max_depth <- depth(model)
  if (max_depth == 0) {
    ## fixed model (and no covariate!)
    glmdata <- prepare_glm(newcov, model$match, 0, newdata, 0)
    result <- glm_likelihood(model$model$model, glmdata$local_mm, glmdata$target)
  } else {
    result <- 0
    discarded <- 0
    for (i in seq_along(newdata)) {
      subtree <- match_context_co(model, ctx)
      the_model <- NULL
      to_keep <- (initial != "specific" && i > ignore) ||
        (initial == "specific" && i > max(max_depth, ignore))
      ## do we have a true match or an extended one?
      if (i >= max_depth + 1) {
        ## true match
        if (subtree$merged) {
          if (verbose) {
            cat("merged model\n")
          }
          the_model <- subtree$tree$merged_model
        } else if (!is.null(subtree$tree[["model"]])) {
          the_model <- subtree$tree$model
        } else {
          stop("No match???")
        }
      } else {
        ## extended match
        if (subtree$merged) {
          if (verbose) {
            cat("merged model\n")
          }
          the_model <- subtree$tree$merged_model
        } else if (!is.null(subtree$tree[["model"]])) {
          if (verbose) {
            cat("extended use of context\n")
          }
          the_model <- subtree$tree$model
        } else if (!is.null(subtree$tree[["extended_model"]])) {
          if (verbose) {
            cat("extended context\n")
          }
          the_model <- subtree$tree$extended_model
        } else {
          stop("No extended match???")
        }
      }
      mm <- prepare_covariate(newcov,
        i - subtree$depth - 1,
        d = the_model$hsize,
        from = subtree$depth - the_model$hsize
      )
      ll <- glm_likelihood(the_model$model, mm, newdata[i])
      if (verbose) {
        cat(
          "[", paste(rev(model$vals[ctx]), collapse = "-"), "] ->",
          paste(model$vals[newdata[i]]), ll,
          the_model$coefficients,
          "\n"
        )
      }
      if (to_keep) {
        result <- result + ll
      } else {
        discarded <- discarded + ll
      }
      ## ctx <- rev(x[(i-max_depth+1):i])
      j <- max(i - max_depth + 1, 1)
      ctx <- x[i:j]
      #      print(paste(i,j))
    }
    result <- as.numeric(result)
  }
  if (verbose) {
    print(as.numeric(discarded))
  }
  attr(result, "nobs") <- max(0, length(x) - ignore)
  if (initial == "truncated") {
    attr(result, "df") <- count_parameters(model, FALSE)
  } else if (initial == "specific") {
    attr(result, "df") <- count_parameters(model, FALSE) + max_depth
  } else {
    attr(result, "df") <- count_parameters(model, TRUE)
  }
  result
}

create_demo_covlmc <- function() {
  withr::local_seed(0)
  x3 <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y3 <- ifelse(runif(length(x3)) > 0.5, c(x3[-1], sample(c("A", "B", "C"), 1)), c(x3[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y3 <- as.factor(ifelse(runif(length(x3)) > 0.2, y3, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  z3 <- data.table::fcase(
    x3[-1] == "A", runif(length(x3) - 1),
    x3[-1] == "B", 0.5 + runif(length(x3) - 1),
    x3[-1] == "C", -0.5 + runif(length(x3) - 1)
  )
  z3 <- c(0, z3)
  df_y3 <- data.frame(y = y3)
  x3_covlmc <- covlmc(x3, df_y3, max_depth = 5, min_size = 5, alpha = 0.01)
  m_cuts <- cutoff(x3_covlmc)
  m_current <- x3_covlmc
  for (k in seq_along(m_cuts[1:4])) {
    m_current <- prune(m_current, m_cuts[k])
  }
  list(model = m_current, rdts = x3, cov = df_y3, full_model = x3_covlmc)
}

create_merged_dataset <- function(n = 1000, seed = 0) {
  withr::local_seed(seed)
  y <- rnorm(n)
  z <- sample(c("K", "L", "M"), n, replace = TRUE, prob = sample(1:4, 3, replace = TRUE))
  cov <- data.frame(y = y, z = z)
  mcov <- model.matrix(~ y + z, cov)
  alpha_1 <- matrix(rnorm(2 * ncol(mcov)), ncol = 2)
  alpha_2 <- matrix(rnorm(2 * ncol(mcov)), ncol = 2)
  prob_1 <- t(apply(mcov %*% alpha_1, 1, \(x) c(1, exp(x)) / sum(exp(x) + 1)))
  prob_2 <- t(apply(mcov %*% alpha_2, 1, \(x) c(1, exp(x)) / sum(exp(x) + 1)))
  res <- rep(sample(1:3, 1), n)
  for (k in 2:n) {
    if (res[k - 1] == 1) {
      res[k] <- sample(1:3, 1, prob = prob_1[k - 1, ])
    } else {
      res[k] <- sample(1:3, 1, prob = prob_2[k - 1, ])
    }
  }
  list(rdts = x, cov = cov)
}
