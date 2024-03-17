covariate_description <- function(covariate) {
  cov_desc <- list()
  cov_size <- 0
  for (var in names(covariate)) {
    if (is.character(covariate[[var]])) {
      covariate[[var]] <- factor(covariate[[var]])
    }
    if (is.factor(covariate[[var]])) {
      cov_desc[[var]] <- levels(covariate[[var]])
      cov_size <- cov_size + length(cov_desc[[var]]) - 1
    } else {
      cov_size <- cov_size + 1
      if (is.logical(covariate[[var]])) {
        cov_desc[[var]] <- c("FALSE", "TRUE")
      }
    }
  }
  list(
    cov_desc = cov_desc,
    cov_logical = lapply(covariate, rlang::is_logical),
    cov_size = cov_size,
    covariate = covariate
  )
}

validate_covariate <- function(model, covariate) {
  assertthat::assert_that(assertthat::has_name(covariate, model$cov_names))
  for (var in names(model$cov_desc)) {
    if (model$cov_logical[[var]]) {
      if (!rlang::is_logical(covariate[[var]])) {
        stop(stringr::str_c(var, " must be logical"))
      }
    } else {
      if (!is.factor(covariate[[var]])) {
        covariate[[var]] <- factor(covariate[[var]])
      }
      new_levels <- match(levels(covariate[[var]]), model$cov_desc[[var]])
      if (anyNA(new_levels)) {
        new_levels <- levels(covariate[[var]])[is.na(new_levels)]
        if (length(new_levels) == 1) {
          str_level <- stringr::str_c(" ", new_levels)
        } else {
          str_level <- stringr::str_c("s ", stringr::str_c(as.character(new_levels), collapse = ", "))
        }
        stop(stringr::str_c("Factor ", var, " has new level", str_level))
      }
    }
  }
  covariate
}
