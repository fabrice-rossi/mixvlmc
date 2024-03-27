test_that("draw obeys its contract for default text output", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
  for (charset in c("ascii", "utf8")) {
    withr::local_options(mixvlmc.charset = charset)
    expect_snapshot_output(draw(rdts_ctree))
    expect_snapshot_output(draw(rdts_ctree, frequency = "total"))
    expect_snapshot_output(draw(rdts_ctree, frequency = "detailed"))
  }
  expect_snapshot_output(draw(rdts_ctree,
    control = draw_control(charset = charset_ascii(
      root = "x",
      open_ct = "[", close_ct = "]",
      first_node = "*",
      final_node = "°",
      vbranch = "^",
      hbranch = "->"
    )), frequency = "detailed"
  ))
})

test_that("draw obeys its contract for LaTeX output", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
  expect_snapshot_output(draw(rdts_ctree, format = "latex"))
  expect_snapshot_output(draw(rdts_ctree, format = "latex", frequency = "total"))
  expect_snapshot_output(draw(rdts_ctree, format = "latex", frequency = "detailed"))
  expect_snapshot_output(draw(rdts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(tabular = FALSE)
  ))
  expect_snapshot_output(draw(rdts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small"
    )
  ))
  expect_snapshot_output(draw(rdts_ctree,
    format = "latex",
    frequency = "detailed",
    control = draw_control(
      tabular = FALSE,
      fontsize = "small",
      orientation = "horizontal",
      decoration = "rectangle"
    )
  ))
  expect_snapshot_output(draw(rdts_ctree,
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
