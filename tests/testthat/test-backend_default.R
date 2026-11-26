test_that("the C++ backend is the default one", {
  ## the default value of the option is set when the package is loaded and
  ## used as the fallback of the backend parameters
  expect_identical(formals(vlmc.default)$backend, quote(getOption("mixvlmc.backend", "C++")))
  withr::local_options(mixvlmc.backend = NULL)
  x <- c(0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1)
  expect_s3_class(vlmc(x), "vlmc_cpp")
  expect_s3_class(ctx_tree(x), "ctx_tree_cpp")
  expect_s3_class(as_vlmc(tune_vlmc(x)), "vlmc_cpp")
})
