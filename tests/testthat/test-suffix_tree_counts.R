test_that("the suffix tree counts correctly the preceeding values of a subsequence", {
  withr::local_seed(10)
  for (k in 1:10) {
    x <- sample(0:k, 1000, replace = TRUE)
    tree <- build_suffix_tree(x[2:length(x)])
    tree$compute_counts(x[1])
    valid_counts <- TRUE
    for (i in 1:200) {
      idx <- sample(2:length(x), 1)
      ss_len <- sample(1:min(5, length(x) - idx + 1), 1)
      subseq <- x[idx:(idx + ss_len - 1)]
      ss_where <- find_occurrences(x, subseq)
      counts <- tabulate(x[ss_where - 1] + 1, nbins = k + 1)
      valid_counts <- identical(counts, tree$counts(subseq))
      if (!valid_counts) {
        break
      }
    }
    expect_true(valid_counts)
  }
})
