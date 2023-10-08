test_that("parent navigation works", {
  withr::local_seed(1)
  dts <- sample(1:5, 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 4)
  all_ok <- TRUE
  for (k in 1:(length(dts) - 2)) {
    for (l in 1:(min(3, length(dts) - 1 - k))) {
      the_ctx <- dts[k:(k + l)]
      the_match <- find_sequence(dts_ctree, the_ctx)
      the_pmatch <- find_sequence(dts_ctree, the_ctx[-1])
      if (!identical(parent(the_match), the_pmatch)) {
        all_ok <- FALSE
        break
      }
    }
    if (!all_ok) {
      break
    }
  }
  expect_true(all_ok)
})

test_that("children navigation works", {
  withr::local_seed(2)
  dts <- sample(1:5, 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 4)
  all_ok <- TRUE
  for (k in 1:(length(dts) - 2)) {
    for (l in 1:(min(3, length(dts) - 1 - k))) {
      the_ctx <- dts[(k + 1):(k + l)]
      the_match <- find_sequence(dts_ctree, the_ctx)
      the_cmatch <- find_sequence(dts_ctree, dts[k:(k + l)])
      the_children <- children(the_match)
      if (!length(the_children) == 5 || !identical(the_children[[dts[k]]], the_cmatch)) {
        all_ok <- FALSE
        break
      }
    }
    if (!all_ok) {
      break
    }
  }
  expect_true(all_ok)
})
