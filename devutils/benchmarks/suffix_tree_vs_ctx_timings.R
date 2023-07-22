x <- sample(0:9, 50000, replace = TRUE)

st_prune_vs_R <- bench::mark(
  build_suffix_tree(x, 10)$prune(2, 1000),
  ctx_tree(x, min_size = 2, max_depth = 1000),
  build_suffix_tree(x, 10)$prune(5, 100),
  ctx_tree(x, min_size = 5, max_depth = 100),
  check = FALSE
)

st_contexts_vs_R <- bench::mark(
  build_suffix_tree(x, 10)$contexts(2, 1000),
  contexts(ctx_tree(x, min_size = 2, max_depth = 1000)),
  build_suffix_tree(x, 10)$contexts(5, 100),
  contexts(ctx_tree(x, min_size = 5, max_depth = 100)),
  check = FALSE
)
