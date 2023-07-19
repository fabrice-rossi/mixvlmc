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
