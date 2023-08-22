build_markov_chain <- function(n, nb_vals, seed = 0) {
  nb_vals <- as.integer(nb_vals)
  withr::local_seed(seed)
  TM <- matrix(NA, ncol = nb_vals, nrow = nb_vals)
  for (k in 1:nb_vals) {
    TM[k, ] <- runif(nb_vals)
    TM[k, ] <- TM[k, ] / sum(TM[k, ])
  }
  x <- rep(0L, n)
  x[1] <- sample(0L:(nb_vals - 1L), 1)
  for (k in 2:n) {
    x[k] <- sample(0L:(nb_vals - 1L), 1, prob = TM[x[k - 1] + 1, ])
  }
  list(x = x, TM = TM)
}

## computes the loglikelihood of a time series x for a model handling initial
## values according to one the following possibilities:
## 1) truncated: from d+1 to length(x)
## 2) specific: from d+1 to length(x) with a ll of 0 for the first d observations
## 3) extended: from 1 to length(x) with assuming global contexts for the first d
##    observations (see the vignette)
slow_loglikelihood <- function(model, x, initial = c("truncated", "specific", "extended"),
                               verbose = FALSE) {
  initial <- match.arg(initial)
  assertthat::assert_that(length(x) >= 1)
  nx <- to_dts(x, model$vals)$ix + 1
  ctx <- c()
  max_depth <- depth(model)
  if (max_depth == 0) {
    ## no model
    result <- sum(table(nx) * log(model$f_by / sum(model$f_by)))
  } else {
    start <- max_depth + 1
    if (initial == "extended") {
      start <- 1
    }
    result <- 0
    if (start <= length(x)) {
      for (i in seq_along(x)) {
        subtree <- match_context(model, ctx)
        if (verbose) {
          cat(paste(
            x[i], "<-",
            stringr::str_pad(paste(model$vals[ctx], collapse = ""), max_depth, side = "right"),
            "matched to",
            stringr::str_pad(paste(model$vals[ctx[1:subtree$depth]], collapse = ""), max_depth, side = "right"),
            "with counts",
            paste(subtree$tree$f_by, collapse = " ")
          ), "\n")
        }
        if (subtree$tree$f_by[nx[i]] == 0) {
          print(paste(i, paste(model$vals[ctx], collapse = "")))
          print(model$vals[nx[i]])
          result <- -Inf
          break
        } else if (i >= start) {
          result <- result + log(subtree$tree$f_by[nx[i]] / sum(subtree$tree$f_by))
        }
        ## ctx <- rev(x[(i-max_depth+1):i])
        j <- max(i - max_depth + 1, 1)
        ctx <- nx[i:j]
        #      print(paste(i,j))
      }
    }
    result <- as.numeric(result)
  }
  if (initial == "truncated") {
    attr(result, "df") <- model$nb_ctx * (length(model$vals) - 1)
    attr(result, "nobs") <- max(0, length(x) - max_depth)
  } else if (initial == "specific") {
    attr(result, "df") <- (model$nb_ctx + max_depth) * (length(model$vals) - 1)
    attr(result, "nobs") <- length(x)
  } else {
    attr(result, "df") <- (model$nb_ctx + count_full_nodes(model)) * (length(model$vals) - 1)
    attr(result, "nobs") <- length(x)
  }
  result
}

matched_vlmc_node2txt <- function(ct, params) {
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    stringr::str_c(
      stringr::str_c(ct[["f_by"]], collapse = ", "),
      " -> ",
      stringr::str_c(ct[["data_f_by"]], collapse = ", ")
    )
  }
}

draw_matched_vlmc <- function(ct) {
  rec_draw(
    "*", "", ct, ct$vals, draw_control(),
    matched_vlmc_node2txt, list()
  )
}
