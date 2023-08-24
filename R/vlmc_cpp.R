#' @export
print.vlmc_cpp <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$vals, collapse = ", ")
  ), "[C++]\n")
  cat(paste(" cutoff: ", signif(x$cutoff, 4), " (quantile: ", x$alpha, ")\n", sep = ""))
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}

#' @rdname prune
#' @export
prune.vlmc_cpp <- function(vlmc, alpha = 0.05, cutoff = NULL, ...) {
  if (extptr_is_null(vlmc$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  if (is.null(cutoff)) {
    if (is.null(alpha) || !is.numeric(alpha) || alpha <= 0 || alpha > 1) {
      stop("the alpha parameter must be in (0, 1]")
    }
  }
  if (is.null(cutoff)) {
    cutoff <- to_native(alpha, length(vlmc$vals))
  } else {
    ## cutoff takes precedence
    alpha <- to_quantile(cutoff, length(vlmc$vals))
  }
  pre_result <- vlmc$root$clone_prune_context(1L, -1L, cutoff)
  result <- new_ctx_tree_cpp(vlmc$vals, pre_result, class = c("vlmc_cpp", "vlmc"))
  result$data_size <- vlmc$data_size
  result$alpha <- alpha
  result$cutoff <- cutoff
  ## recompute the extended_ll
  if (depth(result) > 0) {
    result$ix <- vlmc$ix[1:min(depth(result), length(vlmc$ix))]
    result$extended_ll <- result$root$loglikelihood(result$ix, TRUE, FALSE)
  } else {
    result$extended_ll <- 0
  }
  ## preserve the construction information
  result$max_depth <- vlmc$max_depth
  result
}
