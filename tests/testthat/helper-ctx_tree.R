build_demo_tree_rec <- function(vals, depth) {
  if (depth < 0) {
    NULL
  } else if (depth == 0) {
    list(f_by = rep(2, length(vals)))
  } else {
    children <- vector(mode = "list", length(vals))
    for (v in seq_along(vals)) {
      children[[v]] <- build_demo_tree_rec(vals, depth - 1)
    }
    if (any(sapply(children, is.null))) {
      list()
    } else {
      list(children = children)
    }
  }
}

build_demo_tree <- function(vals, depth) {
  pre_res <- build_demo_tree_rec(vals, depth)
  pre_res$vals <- vals
  new_ctx_tree(vals, pre_res, compute_stats = FALSE)
}
