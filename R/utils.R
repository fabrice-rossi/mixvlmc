to_dts <- function(x, vals = NULL) {
  if (is.null(vals)) {
    if (is.character(x) || is.numeric(x)) {
      vals <- sort(unique(x))
      fx <- factor(x, vals)
    } else if (is.factor(x)) {
      fx <- x
      vals <- factor(levels(fx), levels(fx))
    } else if (is.logical(x)) {
      vals <- c(FALSE, TRUE)
      fx <- factor(x, vals)
    } else {
      stop(paste("x is not character, numeric or factor, but", class(x)))
    }
  } else {
    fx <- factor(x, levels = vals)
    assertthat::assert_that(assertthat::noNA(fx), msg = "x contains unknown states")
    vals <- factor(levels(fx), levels(fx))
  }
  list(ix = as.integer(fx) - 1L, fx = fx, vals = vals)
}

signif_null <- function(x, digits) {
  if (is.null(x) || is.na(x)) {
    "NA"
  } else {
    signif(x, digits)
  }
}

str_c_group <- function(txt, sep, groups, with_rn, rn_sep = sep, fsep = sep) {
  if (length(txt) == 1) {
    txt
  } else {
    if (is.null(groups) || (groups == 0)) {
      if (with_rn) {
        pre_res <- stringr::str_c(txt[1], rn_sep)
        stringr::str_c(pre_res, stringr::str_c(txt[-1], collapse = " "))
      } else {
        stringr::str_c(txt, collapse = sep)
      }
    } else {
      pre_res <- txt[1]
      if (with_rn) {
        grp_size <- (length(txt) - 2) %/% groups
        pre_res <- stringr::str_c(pre_res, txt[2], sep = rn_sep)
        pos <- 3
      } else {
        grp_size <- (length(txt) - 1) %/% groups
        pos <- 2
      }
      for (k in 1:groups) {
        if (k == 1) {
          the_sep <- fsep
        } else {
          the_sep <- sep
        }
        pre_res <- stringr::str_c(pre_res,
          stringr::str_c(txt[pos:(pos + grp_size - 1)],
            collapse = " "
          ),
          sep = the_sep
        )
        pos <- pos + grp_size
      }
      pre_res
    }
  }
}

utf8_pad <- function(string, width, side = c("left", "right")) {
  side <- match.arg(side)
  to_add <- width - cli::utf8_nchar(string, "width")
  to_add_b <- to_add > 0
  res <- string
  if (any(to_add_b)) {
    padding <- stringr::str_pad("", to_add[to_add_b])
    if (side == "left") {
      res[to_add_b] <- stringr::str_c(padding, res[to_add_b])
    } else {
      res[to_add_b] <- stringr::str_c(res[to_add_b], padding)
    }
  }
  res
}


pp_mat <- function(x, digits, width = NULL, sep = NULL, groups = NULL,
                   colnames = NULL, rownames = NULL, rn_sep = sep,
                   first_grp_sep = sep) {
  x_s <- signif(x, digits)
  if (is.matrix(x_s)) {
    x_c <- matrix(apply(x_s, 2, as.character), ncol = ncol(x), nrow = nrow(x))
  } else {
    x_c <- as.character(x_s)
  }
  if (!is.null(colnames)) {
    x_c <- rbind(colnames, x_c)
  }
  if (!is.null(rownames)) {
    if (is.matrix(x_c)) {
      x_c <- cbind(rownames, x_c)
    } else {
      x_c <- c(rownames, x_c)
    }
  }
  if (is.matrix(x_c)) {
    if (is.null(width)) {
      width <- apply(x_c, 2, function(x) max(cli::utf8_nchar(x, "width")))
    }
    if (length(width) > 1) {
      x_pad <- x_c
      for (l in 1:ncol(x_c)) {
        x_pad[, l] <- utf8_pad(x_c[, l], width[l], "right")
      }
    } else {
      x_pad <- apply(x_c, 2, utf8_pad, width, "right")
    }
  } else {
    x_pad <- x_c
  }
  if (is.null(sep)) {
    sep <- " "
  } else {
    assertthat::assert_that(!is.null(groups))
  }
  if (is.matrix(x_pad)) {
    x_rows <- apply(x_pad, 1, str_c_group,
      sep = sep, groups = groups,
      with_rn = !is.null(rownames), rn_sep = rn_sep,
      fsep = first_grp_sep
    )
  } else {
    x_rows <- str_c_group(x_pad, sep, groups, !is.null(rownames), rn_sep,
      fsep = first_grp_sep
    )
  }
  x_rows
}

flex_append <- function(ecur, enew) {
  if (is.null(enew)) {
    ecur
  } else if (is.null(ecur)) {
    enew
  } else {
    if (is.data.frame(enew)) {
      rbind(ecur, enew)
    } else {
      c(ecur, enew)
    }
  }
}

flex_cbind <- function(ecur, enew) {
  if (is.null(enew)) {
    ecur
  } else if (is.null(ecur)) {
    enew
  } else {
    cbind(ecur, enew)
  }
}
