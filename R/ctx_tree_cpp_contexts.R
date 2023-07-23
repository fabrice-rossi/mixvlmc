ctx_recode <- function(ctxs, reverse, vals) {
  if (reverse) {
    lapply(ctxs, function(x) vals[x + 1])
  } else {
    lapply(ctxs, function(x) vals[rev(x) + 1])
  }
}

#' @export
contexts.ctx_tree_cpp <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, frequency = NULL, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  type <- match.arg(type)
  if (is.null(frequency)) {
    pre_res <- ct$root$contexts(1, -1)
    pre_res <- ctx_recode(pre_res, reverse, ct$vals)
    if (type == "auto" || type == "list") {
      pre_res
    } else {
      data.frame(context = I(pre_res))
    }
  } else {
    assertthat::assert_that(type %in% c("auto", "data.frame"))
    assertthat::assert_that(frequency %in% c("total", "detailed"))
    pre_res <- ct$root$detailed_contexts(1, -1)
    pre_res$context <- ctx_recode(pre_res$context, reverse, ct$vals)
    if (frequency == "total") {
      pre_res[1:2]
    } else {
      names(pre_res) <- c("context", "freq", ct$vals)
      pre_res
    }
  }
}
