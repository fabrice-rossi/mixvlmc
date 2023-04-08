prepare_covariate <- function(covariate, ctx_match, d, from, ...) {
  UseMethod("prepare_covariate")
}

#' @exportS3Method
prepare_covariate.matrix <- function(covariate, ctx_match, d, from, with_intercept = FALSE, ...) {
  if (d > 0) {
    ncols <- ncol(covariate) * d
    if (with_intercept) {
      ncols <- ncols + 1
    }
    mm <- matrix(0, nrow = length(ctx_match), ncol = ncols)
    if (with_intercept) {
      mm[, 1] <- 1
    }
    tcol <- ncols
    for (step in 1:d) {
      for (p in ncol(covariate):1) {
        mm[, tcol] <- covariate[ctx_match + from + step, p]
        tcol <- tcol - 1
      }
    }
    mm
  } else {
    matrix(double(), ncol = 0, nrow = 1)
  }
}

#' @exportS3Method
prepare_covariate.data.frame <- function(covariate, ctx_match, d, from, ...) {
  if (d > 0) {
    res <- list()
    the_names <- names(covariate)
    for (step in 1:d) {
      for (p in 1:ncol(covariate)) {
        pre <- covariate[ctx_match + from + d - step + 1, p]
        if (is.list(pre)) {
          pre <- covariate[ctx_match + from + d - step + 1, ][[p]]
        }
        res[[paste0(the_names[p], "_", step)]] <- pre
      }
    }
    list2DF(res)
  } else {
    as.data.frame(matrix(double(), ncol = 0, nrow = 1))
  }
}

prepare_glm <- function(covariate, ctx_match, d, y, from = 0) {
  local_mm <- prepare_covariate(covariate, ctx_match, d, from)
  target <- y[ctx_match + from + d + 1]
  to_keep <- !is.na(target)
  list(local_mm = local_mm[to_keep, , drop = FALSE], target = target[to_keep])
}

sum_log_prob <- function(prob, indic) {
  pre_res <- sum(log(prob)[indic != 0])
  if (length(pre_res) == 0) {
    0
  } else {
    pre_res
  }
}

glm_drop_level_correction <- function(model, newdata) {
  xlevels <- model$xlevels
  if (!is.null(xlevels)) {
    for (var in names(xlevels)) {
      nv <- newdata[[var]]
      if (length(levels(nv)) > length(xlevels[[var]])) {
        to_replace <- is.na(match(newdata[[var]], xlevels[[var]]))
        newdata[[var]][to_replace] <- xlevels[[var]][1]
      }
    }
  }
  newdata
}
