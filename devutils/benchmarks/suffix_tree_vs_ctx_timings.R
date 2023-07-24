x <- sample(0:9, 50000, replace = TRUE)

st_prune_vs_R <- bench::mark(
  ctx_tree(x, min_size = 2, max_depth = 1000, backend = "C++"),
  ctx_tree(x, min_size = 2, max_depth = 1000),
  ctx_tree(x, min_size = 2, max_depth = 1000, backend = "C++", keep_position = FALSE),
  ctx_tree(x, min_size = 2, max_depth = 1000, keep_position = FALSE),
  ctx_tree(x, min_size = 5, max_depth = 100, backend = "C++"),
  ctx_tree(x, min_size = 5, max_depth = 100),
  ctx_tree(x, min_size = 5, max_depth = 100, backend = "C++", keep_position = FALSE),
  ctx_tree(x, min_size = 5, max_depth = 100, keep_position = FALSE),
  check = FALSE
)

st_contexts_vs_R <- bench::mark(
  contexts(ctx_tree(x, min_size = 2, max_depth = 1000, backend = "C++")),
  contexts(ctx_tree(x, min_size = 2, max_depth = 1000)),
  contexts(ctx_tree(x, min_size = 5, max_depth = 100, backend = "C++")),
  contexts(ctx_tree(x, min_size = 5, max_depth = 100)),
  check = FALSE
)
