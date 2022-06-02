prepare_covariate <- function(covariate, ctx_match, d, ...) {
  UseMethod("prepare_covariate")
}

#' @exportS3Method
prepare_covariate.matrix <- function(covariate, ctx_match, d, with_intercept = FALSE, ...) {
  ncols <- ncol(covariate) * d
  if (with_intercept) {
    ncols <- ncols + 1
  }
  mm <- matrix(0, nrow = length(ctx_match), ncol = ncols)
  if (with_intercept) {
    mm[, 1] <- 1
    tcol <- 2
  } else {
    tcol <- 1
  }
  for (step in 1:d) {
    for (p in 1:ncol(covariate)) {
      mm[, tcol] <- covariate[ctx_match + step, p]
      tcol <- tcol + 1
    }
  }
  mm
}

#' @exportS3Method
prepare_covariate.data.frame <- function(covariate, ctx_match, d, ...) {
  res <- list()
  the_names <- names(covariate)
  for (step in 1:d) {
    for (p in 1:ncol(covariate)) {
      res[[paste0(the_names[p], "_", step)]] <- covariate[ctx_match + step, p]
    }
  }
  list2DF(res)
}

prepare_glm <- function(covariate, ctx_match, d, y) {
  local_mm <- prepare_covariate(covariate, ctx_match, d)
  target <- y[ctx_match + d + 1]
  to_keep <- !is.na(target)
  list(local_mm = local_mm[to_keep, , drop = FALSE], target = target[to_keep])
}
