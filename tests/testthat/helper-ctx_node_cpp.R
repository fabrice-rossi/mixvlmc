compare_ctx_node <- function(n1, n2, verbose = TRUE) {
  if (!identical(class(n1), class(n2))) {
    if (verbose) {
      cat("Different classes\n")
    }
    return(FALSE)
  }
  if (inherits(n1, "ctx_node_cpp")) {
    to_check <- c("sequence", "is_context", "rev", "tree")
    for (k in to_check) {
      if (!identical(n1[[k]], n2[[k]])) {
        if (verbose) {
          cat("Difference in", k, "\n")
        }
        return(FALSE)
      }
    }
    identical(n1$node_env$node, n2$node_env$node)
  } else {
    identical(n1, n2)
  }
}
