test_that("Context tree trimming preserve core information ", {
  rdts <- sample(as.factor(c("A", "B", "C")), 1000, replace = TRUE)
  rdts_tree <- ctx_tree(rdts, max_depth = 10, min_size = 5, keep_position = TRUE, backend = "R")
  trimmed_rdts_tree <- trim(rdts_tree)
  expect_identical(
    contexts(trimmed_rdts_tree, frequency = "detailed"),
    contexts(rdts_tree, frequency = "detailed")
  )
  expect_true(object.size(trimmed_rdts_tree) < object.size(rdts_tree))
})

test_that("positions are not available after trimming", {
  x <- c(0, 0, 1, 1, 0, 1)
  ## the root is not a full node for c(x, 2)
  for (y in list(x, c(x, 2))) {
    ct <- trim(ctx_tree(y, min_size = 1, max_depth = 2, backend = "R"))
    expect_error(positions(find_sequence(ct, numeric(0))), "not saved")
    expect_error(positions(find_sequence(ct, 0)), "not saved")
  }
})
