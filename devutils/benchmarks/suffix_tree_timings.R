set.seed(0)

st_create <- bench::mark(
  build_suffix_tree(sample(0:3, 10000, replace = TRUE), 4),
  build_suffix_tree(sample(0:50, 10000, replace = TRUE), 51),
  build_suffix_tree(sample(0:3, 50000, replace = TRUE), 4),
  build_suffix_tree(sample(0:50, 50000, replace = TRUE), 51),
  check = FALSE
)

set.seed(0)

x <- sample(0:9, 100000, replace = TRUE)
x_sft <- build_suffix_tree(x, 10)

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
  build_suffix_tree(sample(0:3, 10000, replace = TRUE), 4)$compute_counts(0, FALSE),
  build_suffix_tree(sample(0:50, 10000, replace = TRUE), 51)$compute_counts(0, FALSE),
  build_suffix_tree(sample(0:3, 50000, replace = TRUE), 4)$compute_counts(0, FALSE),
  build_suffix_tree(sample(0:50, 50000, replace = TRUE), 51)$compute_counts(0, FALSE),
  check = FALSE
)

find_sub_seq <- function(x, nb_vals) {
  tree <- build_suffix_tree(x, nb_vals)
  tree$compute_counts(0, FALSE)
  tree$subsequences(5, 500)
}

st_sub_seq <- bench::mark(
  find_sub_seq(sample(0:3, 10000, replace = TRUE), 4),
  find_sub_seq(sample(0:50, 10000, replace = TRUE), 51),
  find_sub_seq(sample(0:3, 50000, replace = TRUE), 4),
  find_sub_seq(sample(0:50, 50000, replace = TRUE), 51),
  check = FALSE
)

st_ctx <- function(x, nb_vals, with_match) {
  tree <- build_suffix_tree(x, nb_vals)
  tree$compute_counts(0, with_match)
  tree$contexts(2, 1000)
}

st_contexts <- bench::mark(
  st_ctx(sample(0:3, 10000, replace = TRUE), 4, FALSE),
  st_ctx(sample(0:3, 10000, replace = TRUE), 4, TRUE),
  st_ctx(sample(0:50, 10000, replace = TRUE), 51, FALSE),
  st_ctx(sample(0:50, 10000, replace = TRUE), 51, TRUE),
  st_ctx(sample(0:3, 50000, replace = TRUE), 4, FALSE),
  st_ctx(sample(0:3, 50000, replace = TRUE), 4, TRUE),
  st_ctx(sample(0:50, 50000, replace = TRUE), 51, FALSE),
  st_ctx(sample(0:50, 50000, replace = TRUE), 51, TRUE),
  check = FALSE
)
