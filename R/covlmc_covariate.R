covariate_description <- function(covariate) {
  cov_desc <- list()
  for (var in names(covariate)) {
    if (is.character(covariate[[var]])) {
      covariate[[var]] <- factor(covariate[[var]])
    }
    if (is.factor(covariate[[var]])) {
      cov_desc[[var]] <- levels(covariate[[var]])
    }
  }
  list(cov_desc = cov_desc, covariate = covariate)
}

validate_covariate <- function(model, covariate) {
  assertthat::assert_that(assertthat::has_name(covariate, model$cov_names))
  for (var in names(model$cov_desc)) {
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
  covariate
}
