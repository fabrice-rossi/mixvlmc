set.seed(0)

st_create <- bench::mark(
  build_suffix_tree(sample(0:3, 10000, replace = TRUE)),
  build_suffix_tree(sample(0:50, 10000, replace = TRUE)),
  build_suffix_tree(sample(0:3, 50000, replace = TRUE)),
  build_suffix_tree(sample(0:50, 50000, replace = TRUE)),
  check = FALSE
)

set.seed(0)

x <- sample(0:9, 100000, replace = TRUE)
x_sft <- build_suffix_tree(x)

sub_x <- x[2:length(x)]
st_check <- bench::mark(
  x_sft$is_suffix(sub_x),
  check = FALSE
)

st_count <- bench::mark(
  x_sft$count_occurrences(sample(0:9, 1, replace = TRUE)),
  x_sft$count_occurrences(sample(0:9, 2, replace = TRUE)),
  x_sft$count_occurrences(sample(0:9, 3, replace = TRUE)),
  x_sft$count_occurrences(sample(0:9, 4, replace = TRUE)),
  x_sft$count_occurrences(sample(0:9, 5, replace = TRUE)),
  check = FALSE
)

st_all_counts <- bench::mark(
  build_suffix_tree(sample(0:3, 10000, replace = TRUE))$compute_counts(0),
  build_suffix_tree(sample(0:50, 10000, replace = TRUE))$compute_counts(0),
  build_suffix_tree(sample(0:3, 50000, replace = TRUE))$compute_counts(0),
  build_suffix_tree(sample(0:50, 50000, replace = TRUE))$compute_counts(0),
  check = FALSE
)

find_sub_seq <- function(x) {
  tree <- build_suffix_tree(x)
  tree$compute_counts(0)
  tree$subsequences(5, 500)
}

st_sub_seq <- bench::mark(
  find_sub_seq(sample(0:3, 10000, replace = TRUE)),
  find_sub_seq(sample(0:50, 10000, replace = TRUE)),
  find_sub_seq(sample(0:3, 50000, replace = TRUE)),
  find_sub_seq(sample(0:50, 50000, replace = TRUE)),
  check = FALSE
)

st_contexts <- bench::mark(
  build_suffix_tree(sample(0:3, 10000, replace = TRUE))$contexts(2, 1000),
  build_suffix_tree(sample(0:50, 10000, replace = TRUE))$contexts(2, 1000),
  build_suffix_tree(sample(0:3, 50000, replace = TRUE))$contexts(2, 1000),
  build_suffix_tree(sample(0:50, 50000, replace = TRUE))$contexts(2, 1000),
  check = FALSE
)

x <- sample(0:9, 50000, replace = TRUE)

st_contexts_vs_R <- bench::mark(
  build_suffix_tree(x)$contexts(2, 1000),
  contexts(ctx_tree(x, min_size = 2, max_depth = 1000)),
  build_suffix_tree(x)$contexts(5, 100),
  contexts(ctx_tree(x, min_size = 5, max_depth = 100)),
  check = FALSE
)
