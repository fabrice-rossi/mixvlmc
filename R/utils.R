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
  list(ix = as.numeric(fx) - 1, fx = fx, vals = vals)
}

signif_null <- function(x, digits) {
  if (is.null(x) || is.na(x)) {
    "NA"
  } else {
    signif(x, digits)
  }
}

pp_mat <- function(x, digits, width) {
  x_s <- signif(x, digits)
  x_c <- apply(x_s, 2, as.character)
  if (missing(width)) {
    width <- max(stringr::str_length(x_c))
  }
  x_pad <- apply(x_c, 2, stringr::str_pad, width, side = "right")
  x_rows <- apply(x_pad, 1, stringr::str_c, collapse = " ")
  stringr::str_trim(x_rows, "right")
}
