test_that("draw obeys its contract for default ascii output", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  expect_snapshot_output(draw(dts_ctree))
  expect_snapshot_output(draw(dts_ctree, frequency = "total"))
  expect_snapshot_output(draw(dts_ctree, frequency = "detailed"))
  expect_snapshot_output(draw(dts_ctree,
    control = draw_control(
      root = "x",
      open_ct = "[", close_ct = "]",
      first_node = "*",
      final_node = "°",
      vbranch = "^",
      hbranch = "->"
    ), frequency = "detailed"
  ))
})

test_that("draw obeys its contract for LaTeX output", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  expect_snapshot_output(draw(dts_ctree, format = "latex"))
  expect_snapshot_output(draw(dts_ctree, format = "latex", frequency = "total"))
  expect_snapshot_output(draw(dts_ctree, format = "latex", frequency = "detailed"))
  expect_snapshot_output(draw(dts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(tabular = FALSE)
  ))
  expect_snapshot_output(draw(dts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small"
    )
  ))
  expect_snapshot_output(draw(dts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small",
      orientation = "horizontal",
      decoration = "rectangle"
    )
  ))
  expect_snapshot_output(draw(dts_ctree,
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
