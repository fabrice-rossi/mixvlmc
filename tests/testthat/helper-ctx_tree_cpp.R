compare_ctx <- function(ctx1, ctx2, verbose = TRUE) {
  if (!identical(class(ctx1), class(ctx2))) {
    if (verbose) {
      cat("Different classes\n")
    }
    return(FALSE)
  }
  if (is.data.frame(ctx1)) {
    if (!identical(dim(ctx1), dim(ctx2))) {
      if (verbose) {
        cat("Different dimensions\n")
      }
      return(FALSE)
    }
    found <- rep(FALSE, nrow(ctx2))
    for (l in 1:nrow(ctx1)) {
      pos_in_2 <- Position(\(x) identical(x, ctx1$context[[l]]), ctx2$context, nomatch = 0)
      if (pos_in_2 == 0) {
        if (verbose) {
          cat("Context n°", l, "not found\n")
        }
        return(FALSE)
      }
      if (!isTRUE(all.equal(ctx1[l, ], ctx2[pos_in_2, ], check.attributes = FALSE))) {
        if (verbose) {
          print(waldo::compare(ctx1[l, ], ctx2[pos_in_2, ]))
        }
        return(FALSE)
      }
      found[pos_in_2] <- TRUE
    }
    all(found)
  } else {
    ## list case
    if (length(ctx1) != length(ctx2)) {
      if (verbose) {
        cat("Different lengths\n")
      }
      return(FALSE)
    }
    found <- rep(FALSE, length(ctx2))
    for (l in seq_along(ctx1)) {
      pos_in_2 <- Position(\(x) identical(x, ctx1[[l]]), ctx2, nomatch = 0)
      if (pos_in_2 == 0) {
        if (verbose) {
          cat("Context n°", l, "not found\n")
        }
        return(FALSE)
      }
      found[pos_in_2] <- TRUE
    }
    all(found)
  }
}
