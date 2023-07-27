ctx_recode <- function(ctxs, reverse, vals) {
  if (reverse) {
    lapply(ctxs, function(x) vals[x + 1])
  } else {
    lapply(ctxs, function(x) vals[rev(x) + 1])
  }
}

#' @export
contexts.ctx_tree_cpp <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, frequency = NULL, positions = FALSE, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  type <- match.arg(type)
  if (is.null(frequency) && !positions) {
    pre_res <- ct$root$contexts(1, -1)
    pre_res <- ctx_recode(pre_res, reverse, ct$vals)
    if (type == "auto" || type == "list") {
      pre_res
    } else {
      data.frame(context = I(pre_res))
    }
  } else {
    assertthat::assert_that(type %in% c("auto", "data.frame"))
    with_freq <- FALSE
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
      with_freq <- frequency == "detailed"
    }
    pre_res <- ct$root$detailed_contexts(1, -1, with_freq, positions)
    pre_res$context <- I(ctx_recode(pre_res$context, reverse, ct$vals))
    if (positions) {
      pre_res$positions <- I(pre_res$positions)
    }
    if (is.null(frequency)) {
      pre_res[-2]
    } else {
      if (frequency == "detailed") {
        names_pre_res <- names(pre_res)
        names_pre_res[3:(2 + length(ct$vals))] <- ct$vals
        names(pre_res) <- names_pre_res
      }
      pre_res
    }
  }
}
