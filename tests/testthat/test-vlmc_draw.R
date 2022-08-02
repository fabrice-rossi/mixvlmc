test_that("draw obeys its contract", {
  data_set <- build_markov_chain(1000, 3, seed = 0)
  x_vlmc <- vlmc(data_set$x)
  expect_snapshot_output(draw(x_vlmc))
  expect_snapshot_output(draw(x_vlmc, node2txt = NULL))
  expect_snapshot_output(draw(x_vlmc, prob = TRUE))
})
