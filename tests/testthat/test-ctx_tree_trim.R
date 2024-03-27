test_that("Context tree trimming preserve core information ", {
  rdts <- sample(as.factor(c("A", "B", "C")), 1000, replace = TRUE)
  rdts_tree <- ctx_tree(rdts, max_depth = 10, min_size = 5, keep_position = TRUE)
  trimmed_rdts_tree <- trim(rdts_tree)
  expect_identical(
    contexts(trimmed_rdts_tree, type = "data.frame", frequency = "detailed"),
    contexts(rdts_tree, type = "data.frame", frequency = "detailed")
  )
  expect_true(object.size(trimmed_rdts_tree) < object.size(rdts_tree))
})
