test_that("the suffix tree extracts all subsequences", {
  withr::local_seed(10)
  x <- sample(0:3, 51, replace = TRUE)
  tree <- build_suffix_tree(x[2:length(x)], 4)
  tree$compute_counts(x[1], FALSE)
  ## all subsequences
  all_ss <- tree$subsequences(1, -1)
  all_ss_matched <- rep(FALSE, length(all_ss))
  all_in <- TRUE
  for (k in 2:length(x)) {
    for (l in k:length(x)) {
      sub_seq <- x[k:l]
      pos <- Position(\(x) identical(x, sub_seq), all_ss, nomatch = 0)
      if (pos == 0) {
        all_in <- FALSE
        break
      } else {
        all_ss_matched[pos] <- TRUE
      }
    }
  }
  expect_true(all_in)
  expect_true(all(all_ss_matched))
})

test_that("the suffix tree extracts subsequences with minimal counts", {
  withr::local_seed(10)
  x <- sample(0:3, 51, replace = TRUE)
  tree <- build_suffix_tree(x[2:length(x)], 4)
  tree$compute_counts(x[1], FALSE)
  ## at least 2
  all_ss <- tree$subsequences(2, -1)
  all_ss_matched <- rep(FALSE, length(all_ss))
  all_in <- TRUE
  for (k in 2:length(x)) {
    for (l in k:length(x)) {
      sub_seq <- x[k:l]
      if (count_occurrences(x[2:length(x)], sub_seq) >= 2) {
        pos <- Position(\(x) identical(x, sub_seq), all_ss, nomatch = 0)
        if (pos == 0) {
          all_in <- FALSE
          break
        } else {
          all_ss_matched[pos] <- TRUE
        }
      }
    }
    if (!all_in) {
      break
    }
  }
  expect_true(all_in)
  expect_true(all(all_ss_matched))
})

test_that("the suffix tree extracts subsequences with maximal length", {
  withr::local_seed(10)
  x <- sample(0:3, 51, replace = TRUE)
  tree <- build_suffix_tree(x[2:length(x)], 4)
  tree$compute_counts(x[1], FALSE)
  ## max length 5
  max_length <- 5
  all_ss <- tree$subsequences(1, max_length)
  all_ss_matched <- rep(FALSE, length(all_ss))
  all_in <- TRUE
  for (k in 2:length(x)) {
    for (l in k:(min(length(x), k + max_length - 1))) {
      sub_seq <- x[k:l]
      pos <- Position(\(x) identical(x, sub_seq), all_ss, nomatch = 0)
      if (pos == 0) {
        all_in <- FALSE
        break
      } else {
        all_ss_matched[pos] <- TRUE
      }
    }
    if (!all_in) {
      break
    }
  }
  expect_true(all_in)
  expect_true(all(all_ss_matched))
})

test_that("the suffix tree extracts subsequences with minimal counts and maximal length", {
  withr::local_seed(10)
  x <- sample(0:3, 501, replace = TRUE)
  tree <- build_suffix_tree(x[2:length(x)], 4)
  tree$compute_counts(x[1], FALSE)
  max_length <- 8
  min_count <- 3
  all_ss <- tree$subsequences(min_count, max_length)
  all_ss_matched <- rep(FALSE, length(all_ss))
  all_in <- TRUE
  for (k in 2:length(x)) {
    for (l in k:(min(length(x), k + max_length - 1))) {
      sub_seq <- x[k:l]
      if (count_occurrences(x[2:length(x)], sub_seq) >= min_count) {
        pos <- Position(\(x) identical(x, sub_seq), all_ss, nomatch = 0)
        if (pos == 0) {
          all_in <- FALSE
          break
        } else {
          all_ss_matched[pos] <- TRUE
        }
      }
    }
    if (!all_in) {
      break
    }
  }
  expect_true(all_in)
  expect_true(all(all_ss_matched))
})
