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
