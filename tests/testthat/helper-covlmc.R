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
  dts_cov <- data.frame(y = y, z = z)
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
  list(x = x, covariate = dts_cov)
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
  elec_dts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
  elec_tune <- tune_covlmc(elec_dts, elec_cov, min_size = 5)
  elec_model <- prune(as_covlmc(elec_tune), alpha = 3.961e-10)
  result <- list(model = elec_model, dts = elec_dts, cov = elec_cov)
  if (with_new_data) {
    pc_week_17_18 <- powerconsumption[powerconsumption$week %in% c(17, 18), ]
    elec_new_dts <- cut(pc_week_17_18$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
    elec_new_cov <- data.frame(day = (pc_week_17_18$hour >= 7 & pc_week_17_18$hour <= 18))
    result$new_cov <- elec_new_cov
    result$new_dts <- elec_new_dts
  }
  result
}
