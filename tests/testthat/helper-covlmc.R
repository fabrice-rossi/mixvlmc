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
          if (is.null(df)) {
            df <- sub_p
          } else {
            df <- rbind(df, sub_p)
          }
        }
      }
      df
    }
  }
  recurse_extract_p_value(tree)
}
