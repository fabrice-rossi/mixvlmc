test_that("draw obeys its contract for default text output", {
  data_set <- build_markov_chain(1000, 3, seed = 0)
  x_vlmc <- vlmc(data_set$x)
  for (charset in c("ascii", "utf8")) {
    withr::local_options(mixvlmc.charset = charset)
    expect_snapshot_output(draw(x_vlmc))
    expect_snapshot_output(draw(x_vlmc, prob = NULL))
    expect_snapshot_output(draw(x_vlmc, prob = FALSE))
  }
})

test_that("draw obeys its contract for latex output", {
  data_set <- build_markov_chain(1000, 3, seed = 0)
  x_vlmc <- vlmc(data_set$x)
  expect_snapshot_output(draw(x_vlmc, format = "latex"))
  expect_snapshot_output(draw(x_vlmc, format = "latex", prob = NULL))
  expect_snapshot_output(draw(x_vlmc, format = "latex", prob = FALSE))
  expect_snapshot_output(draw(x_vlmc,
    format = "latex",
    frequency = "detailed",
    control = draw_control(tabular = FALSE)
  ))
  expect_snapshot_output(draw(x_vlmc,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small"
    )
  ))
  expect_snapshot_output(draw(x_vlmc,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small",
      orientation = "horizontal",
      decoration = "rectangle"
    )
  ))
  expect_snapshot_output(draw(x_vlmc,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      fontsize = "small",
      prob_fontsize = "scriptsize",
      orientation = "horizontal",
      decoration = "circle"
    )
  ))
})
