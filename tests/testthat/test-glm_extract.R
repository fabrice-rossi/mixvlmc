test_that("glm_coef works for all type of variables", {
  d_size <- 50
  dataset <- data.frame(
    x = rnorm(d_size),
    y = sample(c(TRUE, FALSE), d_size, replace = TRUE),
    z = sample(factor(c("x", "y", "z")), d_size, replace = TRUE)
  )
  dataset$target <- sample(factor(c("0", "1")), d_size, replace = TRUE)
  glm_model <- glm(target ~ ., family = binomial(), data = dataset)
  e_coef <- glm_coef(glm_model, dataset)
  expect_equal(e_coef, stats::coef(glm_model), ignore_attr = TRUE)
})
