to_dts <- function(x, vals = NULL) {
  if (is.null(vals)) {
    if (is.character(x) || is.numeric(x)) {
      fx <- as.factor(x)
    } else if (is.factor(x)) {
      fx <- x
    } else {
      stop(paste("x is not character, numeric or factor, but", class(x)))
    }
    vals <- levels(fx)
  } else {
    fx <- factor(x, levels = vals)
    assertthat::assert_that(assertthat::noNA(fx), msg = "x contains unknown states")
  }
  list(ix = as.numeric(fx) - 1, vals = vals)
}
