#' @export
restore_model.vlmc_cpp <- function(tree) {
  if (extptr_is_null(tree$root$.pointer)) {
    cpp_tree <- build_suffix_tree(
      tree$restoration$rev_x,
      tree$restoration$max_x + 1
    )
    cpp_tree$compute_counts(tree$restoration$last_value, tree$keep_match)
    if (tree$pruned) {
      cpp_tree$prune_context(
        tree$restoration$min_size,
        tree$restoration$max_depth,
        tree$cutoff
      )
    } else {
      cpp_tree$prune(tree$restoration$min_size, tree$restoration$max_depth)
    }
    cpp_tree$make_explicit()
    cpp_tree$compute_reverse()
    tree$root@.xData$.pointer <- cpp_tree@.xData$.pointer
    tree$root@.xData$.cppclass <- cpp_tree@.xData$.cppclass
    tree$root@.xData$.module <- cpp_tree@.xData$.module
    ## we need to unbind all the functions in .xData to avoid issues
    content <- rlang::env_names(tree$root@.xData)
    internals <- stringr::str_starts(content, "\\.")
    to_keep <- c("compute_counts", "prune_context", "make_explicit",
                 "compute_reverse", "getClass", "initialize", "finalize")
    to_remove <- setdiff(content[!internals], to_keep)
    for (fn in to_remove) {
      rlang::env_unbind(tree$root@.xData, fn)
    }
  }
}

#' @export
print.vlmc_cpp <- function(x, ...) {
  restore_model(x)
  cat(paste(
    "VLMC context tree on",
    paste(x$vals, collapse = ", ")
  ), "[C++]\n")
  cat(paste(" cutoff: ", signif(x$cutoff, 4), " (quantile: ", signif(x$alpha, 4), ")\n", sep = ""))
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
  restore_model(vlmc)
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
  ## preserve the construction information
  result$max_depth <- vlmc$max_depth
  result$restoration <- result$root$restoration_info()
  result$alpha <- alpha
  result$cutoff <- cutoff
  ## recompute the extended_ll
  if (depth(result) > 0) {
    result$ix <- vlmc$ix[1:min(depth(result), length(vlmc$ix))]
    result$extended_ll <- result$root$loglikelihood(result$ix, 0, TRUE, FALSE)
  } else {
    result$extended_ll <- 0
  }
  result$keep_match <- vlmc$keep_match
  result$data_size <- vlmc$data_size
  result$pruned <- TRUE
  result
}

#' @rdname cutoff.vlmc
#' @export
cutoff.vlmc_cpp <- function(model, scale = c("quantile", "native"), raw = FALSE,
                            tolerance = .Machine$double.eps^0.5, ...) {
  restore_model(model)
  scale <- match.arg(scale)
  pre_result <- relaxed_unique(model$root$cutoff(), tolerance)
  guaranteed_pruning(pre_result, length(model$vals), scale, raw)
}
