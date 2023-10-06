test_that("Context tree trimming preserve core information ", {
  dts <- sample(as.factor(c("A", "B", "C")), 1000, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 10, min_size = 5, keep_position = TRUE)
  trimmed_dts_tree <- trim(dts_tree)
  expect_identical(
    contexts(trimmed_dts_tree, type = "data.frame", frequency = "detailed"),
    contexts(dts_tree, type = "data.frame", frequency = "detailed")
  )
  expect_true(object.size(trimmed_dts_tree) < object.size(dts_tree))
})
