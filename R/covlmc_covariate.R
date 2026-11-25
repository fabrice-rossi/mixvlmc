covariate_description <- function(covariate) {
  cov_desc <- list()
  cov_size <- 0
  for (var in names(covariate)) {
    if (is.character(covariate[[var]])) {
      covariate[[var]] <- factor(covariate[[var]])
    }
    if (is.factor(covariate[[var]])) {
      ## unused levels do not correspond to parameters
      covariate[[var]] <- droplevels(covariate[[var]])
      cov_desc[[var]] <- levels(covariate[[var]])
      cov_size <- cov_size + length(cov_desc[[var]]) - 1
    } else {
      cov_size <- cov_size + 1
    }
  }
  list(cov_desc = cov_desc, cov_size = cov_size, covariate = covariate)
}

validate_covariate <- function(model, covariate) {
  assertthat::assert_that(assertthat::has_name(covariate, model$cov_names))
  if (anyNA(covariate[model$cov_names])) {
    stop("covariates cannot contain missing values")
  }
  for (var in names(model$cov_desc)) {
    if (!is.factor(covariate[[var]])) {
      covariate[[var]] <- factor(covariate[[var]])
    }
    ## unused levels are discarded
    used_levels <- levels(droplevels(covariate[[var]]))
    new_levels <- match(used_levels, model$cov_desc[[var]])
    if (anyNA(new_levels)) {
      new_levels <- used_levels[is.na(new_levels)]
      if (length(new_levels) == 1) {
        str_level <- stringr::str_c(" ", new_levels)
      } else {
        str_level <- stringr::str_c("s ", stringr::str_c(as.character(new_levels), collapse = ", "))
      }
      stop(stringr::str_c("Factor ", var, " has new level", str_level))
    }
    covariate[[var]] <- factor(covariate[[var]], levels = model$cov_desc[[var]])
  }
  covariate
}
