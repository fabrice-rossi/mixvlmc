test_that("predictions do not depend off the back end", {
  for (k in 2:5) {
    data_set <- build_markov_chain(1000, k, seed = k)
    cpp_vlmc <- vlmc(data_set$x, backend = "C++")
    r_vlmc <- vlmc(data_set$x, backend = "R")
    expect_identical(predict(cpp_vlmc, data_set$x), predict(r_vlmc, data_set$x))
    expect_identical(
      predict(cpp_vlmc, data_set$x, type = "probs"),
      predict(r_vlmc, data_set$x, type = "probs")
    )
  }
})
