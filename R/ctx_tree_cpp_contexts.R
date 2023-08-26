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
    }
    pre_res <- ct$root$full_contexts(1, -1, positions, FALSE, FALSE)
    res <- data.frame(context = I(ctx_recode(pre_res$context, reverse, ct$vals)))
    if (!is.null(frequency)) {
      if (frequency == "detailed") {
        res <- cbind(res, pre_res$counts)
        names(res)[3:(2 + length(ct$vals))] <- ct$vals
      } else {
        res$freq <- pre_res$counts$freq
      }
    }
    if (positions) {
      res$positions <- I(pre_res$positions)
    }
    res
  }
}
