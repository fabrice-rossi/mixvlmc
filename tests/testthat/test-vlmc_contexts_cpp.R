test_that("contexts do not depend on the backend", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 500, replace = TRUE)
    cpp_model <- vlmc(x, alpha = 0.2, backend = "C++", keep_match = TRUE)
    cpp_ctxs <- contexts(cpp_model, frequency = "detailed", position = TRUE, cutoff = "native", metrics = TRUE)
    r_model <- vlmc(x, alpha = 0.2, keep_match = TRUE, backend = "R")
    r_ctxs <- contexts(r_model, frequency = "detailed", position = TRUE, cutoff = "native", metrics = TRUE)
    expect_true(compare_ctx(r_ctxs, cpp_ctxs))
  }
})

test_that("detailed frequencies are named by states", {
  withr::local_seed(0)
  for (x in list(
    factor(sample(c("a", "b", "c"), 200, replace = TRUE)),
    sample(c("a", "b", "c"), 200, replace = TRUE),
    sample(c(3, 5, 7), 200, replace = TRUE)
  )) {
    for (model in list(ctx_tree(x, max_depth = 2, backend = "R"), vlmc(x, alpha = 0.5, backend = "R"))) {
      model_cpp <- switch(class(model)[1],
        ctx_tree = ctx_tree(x, max_depth = 2, backend = "C++"),
        vlmc(x, alpha = 0.5, backend = "C++")
      )
      expect_identical(
        names(contexts(model_cpp, frequency = "detailed")),
        names(contexts(model, frequency = "detailed"))
      )
    }
  }
})
