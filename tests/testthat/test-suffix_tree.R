test_that("the suffix tree contains all the suffixes", {
  withr::local_seed(10)
  for (k in 1:10) {
    x <- sample(0:k, 1000, replace = TRUE)
    tree <- build_suffix_tree(x, k + 1)
    all_suffixes <- TRUE
    for (i in seq_along(x)) {
      if (!tree$is_suffix(x[i:length(x)])) {
        all_suffixes <- FALSE
        break
      }
    }
    expect_true(all_suffixes)
  }
})

test_that("the suffix tree does not contain non suffix", {
  withr::local_seed(10)
  for (k in 1:10) {
    x <- sample(0:k, 1000, replace = TRUE)
    tree <- build_suffix_tree(x, k + 1)
    all_ok <- TRUE
    for (l in 1:998) {
      # let's remove an integer randomly
      pre_suffix <- x[l:1000]
      pre_suffix <- pre_suffix[-sample(2:length(pre_suffix), 1)]
      if (identical(pre_suffix, x[(l + 1):1000])) {
        all_ok <- all_ok & tree$is_suffix(pre_suffix)
      } else {
        all_ok <- all_ok & (!tree$is_suffix(pre_suffix))
      }
      # test an almost suffix
      pre_suffix <- x[l:999]
      if (identical(pre_suffix, x[(l + 1):1000])) {
        all_ok <- all_ok & tree$is_suffix(pre_suffix)
      } else {
        all_ok <- all_ok & (!tree$is_suffix(pre_suffix))
      }
      if (!all_ok) {
        break
      }
    }
    expect_true(all_ok)
  }
})

test_that("the suffix tree counts correctly subsequences", {
  withr::local_seed(10)
  for (k in 1:10) {
    x <- sample(0:k, 100 * (ceiling(k / 3)), replace = TRUE)
    tree <- build_suffix_tree(x, k + 1)
    xt <- tabulate(x, nbins = k + 1)
    for (j in 1:k) {
      expect_equal(tree$count_occurrences(j), xt[j])
    }
    all_ok <- TRUE
    for (l in 1:50) {
      pos <- sample(1:(length(x) / 2), 1)
      pos_end <- sample((pos + 1):length(x), 1)
      if (tree$count_occurrences(x[pos:pos_end]) !=
        count_occurrences(x, x[pos:pos_end])) {
        all_ok <- FALSE
        break
      }
      tmp <- sample(0:k, length(x) / 10, replace = TRUE)
      if (tree$count_occurrences(tmp) !=
        count_occurrences(x, tmp)) {
        all_ok <- FALSE
        break
      }
    }
  }
})

test_that("error cases are correctly reported", {
  withr::local_seed(11)
  x <- sample(0:2, 100, replace = TRUE)
  expect_error(build_suffix_tree(x, 2))
  x <- c(x, -1L)
  expect_error(build_suffix_tree(x, 3))
})
