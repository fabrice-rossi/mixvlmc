restore_ctx_node_cpp <- function(node) {
  restore_model(node$tree)
  if (extptr_is_null(node$node_env$node)) {
    if (length(node$sequence) > 0) {
      nx <- to_dts(node$sequence, node$tree$vals)
      node$node_env$node <- node$tree$root$raw_find_sequence(nx$ix)
    } else {
      node$node_env$node <- node$tree$root$raw_find_sequence(integer())
    }
  }
}

#' Create `ctx_node_cpp`
#'
#' @param ctx the sequence in reverse order
#' @param tree the tree
#' @param node the node representing the context
#' @param rev whether the sequence should be reported in reverse order or in
#'   temporal order
#' @param ... additional parameters
#' @param class finer class
#' @noRd
new_ctx_node_cpp <- function(ctx, tree, node, rev, ..., class = character()) {
  node_env <- rlang::new_environment(list(node = node))
  structure(
    list(
      sequence = ctx, node_env = node_env, tree = tree,
      is_context = tree$root$node_is_context(node),
      rev = rev, ...
    ),
    class = c(class, "ctx_node_cpp", "ctx_node")
  )
}

#' @export
print.ctx_node_cpp <- function(x, ...) {
  restore_ctx_node_cpp(x)
  if (x$is_context) {
    cat("Context")
  } else {
    cat("Sequence")
  }
  if (x$rev) {
    cat(" [R]: ")
    x_seq <- x$sequence
  } else {
    cat(" [T]: ")
    x_seq <- rev(x$sequence)
  }
  cat(paste(x_seq, collapse = ", "), "\n")
  counts <- x$tree$root$node_counts(x$node_env$node)
  cat(" followed by ", paste(paste(x$tree$vals, counts, sep = " ("), collapse = "), "), ")\n", sep = "")
}


#' @export
#' @rdname find_sequence
find_sequence.ctx_tree_cpp <- function(ct, ctx, reverse = FALSE, ...) {
  restore_model(ct)
  if (length(ctx) == 0) {
    if (isTRUE(ct$keep_match) && is.null(ct$match)) {
      ct$match <- 1:ct$data_size
    }
    root <- ct$root$raw_find_sequence(integer())
    new_ctx_node_cpp(ctx, ct, root, reverse)
  } else {
    assertthat::assert_that((typeof(ctx) == typeof(ct$vals)) && methods::is(ctx, class(ct$vals)),
      msg = "ctx is not compatible with the model state space"
    )
    if (!reverse) {
      ctx <- rev(ctx)
    }
    nx <- to_dts(ctx, ct$vals)
    node <- ct$root$raw_find_sequence(nx$ix)
    if (extptr_is_null(node)) {
      NULL
    } else {
      new_ctx_node_cpp(ctx, ct, node, reverse)
    }
  }
}

#' @export
#' @rdname positions
positions.ctx_node_cpp <- function(node) {
  restore_ctx_node_cpp(node)
  if (!node$tree$root$has_positions) {
    stop("Cannot report positions if they were not saved")
  }
  node$tree$data_size - node$tree$root$node_positions(node$node_env$node) - 1L
}

#' @export
#' @rdname counts
counts.ctx_node_cpp <- function(node,
                                frequency = c("detailed", "total"),
                                local = FALSE) {
  restore_ctx_node_cpp(node)
  frequency <- match.arg(frequency)
  assertthat::assert_that(rlang::is_logical(local))
  if (!local) {
    freqs <- node$tree$root$node_counts(node$node_env$node)
  } else {
    freqs <- node$tree$root$node_local_counts(node$node_env$node)
  }
  if (frequency == "total") {
    sum(freqs)
  } else {
    freq_by_val <- as.list(freqs)
    names(freq_by_val) <- as.character(node$tree$vals)
    freq_by_val <- c(list(total = sum(freqs)), freq_by_val)
    data.frame(freq_by_val, check.names = FALSE)
  }
}
