test_that(".onLoad does not modified prespecified options", {
  withr::local_options(mixvlmc.predictive = "foo")
  mixvlmc:::.onLoad()
  expect_equal(options()$mixvlmc.predictive, "foo")
})

test_that(".onLoad sets default options", {
  withr::local_options(mixvlmc.predictive = NULL)
  mixvlmc:::.onLoad()
  expect_equal(options()$mixvlmc.predictive, "glm")
})
