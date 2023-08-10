test_that("C++ based vlmc draw produces identical results as the R one", {
  data_set <- build_markov_chain(1000, 3, seed = 0)
  x_vlmc <- vlmc(data_set$x)
  x_vlmc_cpp <- vlmc(data_set$x, backend = "C++")
  expect_true(compare_output(
    {
      draw(x_vlmc)
      draw(x_vlmc, prob = NULL)
      draw(x_vlmc, prob = FALSE)
    },
    {
      draw(x_vlmc_cpp)
      draw(x_vlmc_cpp, prob = NULL)
      draw(x_vlmc_cpp, prob = FALSE)
    }
  ))
  skip_on_ci()
  skip_on_cran()
  for (k in 1:5) {
    dts <- sample(0:k, 100, replace = TRUE)
    r_vlmc <- vlmc(dts, min_size = 1, max_depth = 8, alpha = 0.5)
    cpp_vlmc <- vlmc(dts, min_size = 1, max_depth = 8, alpha = 0.5, backend = "C++")
    expect_true(compare_output(
      {
        draw(r_vlmc)
        draw(r_vlmc, prob = NULL)
        draw(r_vlmc, prob = FALSE)
      },
      {
        draw(cpp_vlmc)
        draw(cpp_vlmc, prob = NULL)
        draw(cpp_vlmc, prob = FALSE)
      }
    ))
  }
})
