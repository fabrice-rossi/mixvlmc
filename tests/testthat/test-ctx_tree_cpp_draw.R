test_that("C++ based context draw produces identical results as the R one", {
  skip_on_ci()
  skip_on_cran()
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  r_tree <- ctx_tree(rdts, min_size = 2)
  cpp_tree <- ctx_tree(rdts, min_size = 2, keep_position = FALSE, backend = "C++")
  expect_true(compare_output(
    {
      exercise_draw(cpp_tree)
    },
    {
      exercise_draw(r_tree)
    }
  ))
  for (k in 1:5) {
    rdts <- sample(0:k, 40, replace = TRUE)
    r_tree <- ctx_tree(rdts, min_size = 1, max_depth = 8)
    cpp_tree <- ctx_tree(rdts, min_size = 1, max_depth = 8, keep_position = FALSE, backend = "C++")
    expect_true(compare_output(
      {
        exercise_draw(cpp_tree)
      },
      {
        exercise_draw(r_tree)
      }
    ))
  }
})
