to_dts <- function(x, vals = NULL) {
  if (is.null(vals)) {
    if (is.character(x) || is.numeric(x)) {
      vals <- sort(unique(x))
      fx <- factor(x, vals)
    } else if (is.factor(x)) {
      fx <- x
      vals <- factor(levels(fx), levels(fx))
    } else {
      stop(paste("x is not character, numeric or factor, but", class(x)))
    }
  } else {
    fx <- factor(x, levels = vals)
    assertthat::assert_that(assertthat::noNA(fx), msg = "x contains unknown states")
    vals <- factor(levels(fx), levels(fx))
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

str_c_group <- function(txt, sep, groups) {
  if (length(txt) == 1) {
    txt
  } else {
    pre_res <- txt[1]
    grp_size <- (length(txt) - 1) %/% groups
    pos <- 2
    for (k in 1:groups) {
      pre_res <- stringr::str_c(pre_res, stringr::str_c(txt[pos:(pos + grp_size - 1)], collapse = " "), sep = sep)
      pos <- pos + grp_size
    }
    pre_res
  }
}

pp_mat <- function(x, digits, width = NULL, sep = NULL, groups = NULL) {
  x_s <- signif(x, digits)
  if (is.matrix(x_s)) {
    x_c <- matrix(apply(x_s, 2, as.character), ncol = ncol(x), nrow = nrow(x))
    if (is.null(width)) {
      width <- apply(x_c, 2, function(x) max(stringr::str_length(x)))
    }
    if (length(width) > 1) {
      x_pad <- x_c
      for (l in 1:ncol(x_c)) {
        x_pad[, l] <- stringr::str_pad(x_c[, l], width[l], side = "right")
      }
    } else {
      x_pad <- apply(x_c, 2, stringr::str_pad, width, side = "right")
    }
    if (is.null(sep)) {
      x_rows <- apply(x_pad, 1, stringr::str_c, collapse = " ")
    } else {
      assertthat::assert_that(!is.null(groups))
      x_rows <- apply(x_pad, 1, str_c_group, sep = sep, groups = groups)
    }
  } else {
    x_c <- as.character(x_s)
    if (is.null(sep)) {
      x_rows <- stringr::str_c(x_c, collapse = " ")
    } else {
      assertthat::assert_that(!is.null(groups))
      x_rows <- str_c_group(x_c, sep, groups)
    }
  }
  stringr::str_trim(x_rows, "right")
}
