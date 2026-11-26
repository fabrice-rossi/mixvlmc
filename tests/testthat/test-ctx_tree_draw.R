test_that("draw obeys its contract for default text output", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2, backend = "R")
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
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2, backend = "R")
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

test_that("LaTeX drawing without tabular respects the frequency parameter", {
  x <- c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  for (backend in c("R", "C++")) {
    ct <- ctx_tree(x, min_size = 1, max_depth = 2, backend = backend)
    ctrl <- draw_control(tabular = FALSE)
    no_freq <- paste(capture.output(draw(ct, "latex", control = ctrl)), collapse = "\n")
    expect_false(grepl("(10)", no_freq, fixed = TRUE))
    expect_false(grepl("(5,5)", no_freq, fixed = TRUE))
    total <- paste(capture.output(draw(ct, "latex", control = ctrl, frequency = "total")), collapse = "\n")
    expect_true(grepl("(10)", total, fixed = TRUE))
    expect_false(grepl("(5,5)", total, fixed = TRUE))
    detailed <- paste(capture.output(draw(ct, "latex", control = ctrl, frequency = "detailed")), collapse = "\n")
    expect_true(grepl("(5,5)", detailed, fixed = TRUE))
  }
})
