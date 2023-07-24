test_that("the suffix tree finds the positions of a subsequence", {
  withr::local_seed(10)
  for (k in 1:10) {
    x <- sample(0:k, 1000, replace = TRUE)
    tree <- build_suffix_tree(x[2:length(x)], nb_vals = k + 1)
    tree$compute_counts(x[1], TRUE)
    valid_positions <- TRUE
    for (i in 1:200) {
      idx <- sample(2:length(x), 1)
      ss_len <- sample(1:min(5, length(x) - idx + 1), 1)
      subseq <- x[idx:(idx + ss_len - 1)]
      ss_where <- find_occurrences(x[-1], subseq)
      positions <- 1 + sort(tree$positions(subseq))
      valid_positions <- identical(positions, ss_where)
      if (!valid_positions) {
        break
      }
    }
    expect_true(valid_positions)
  }
})
