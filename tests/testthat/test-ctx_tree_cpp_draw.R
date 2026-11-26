test_that("C++ based context draw produces identical results as the R one", {
  skip_on_ci()
  skip_on_cran()
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  r_tree <- ctx_tree(rdts, min_size = 2, backend = "R")
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
    r_tree <- ctx_tree(rdts, min_size = 1, max_depth = 8, backend = "R")
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

test_that("C++ based context draw reports the frequencies as the R one", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  r_tree <- ctx_tree(rdts, min_size = 1, max_depth = 2, backend = "R")
  cpp_tree <- ctx_tree(rdts, min_size = 1, max_depth = 2, backend = "C++")
  for (frequency in c("total", "detailed")) {
    expect_identical(
      format(draw(cpp_tree, frequency = frequency)),
      format(draw(r_tree, frequency = frequency))
    )
  }
})
