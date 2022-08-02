test_that("draw obeys its contract", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  expect_snapshot_output(draw(dts_ctree))
  expect_snapshot_output(draw(dts_ctree, node2txt = NULL))
  expect_snapshot_output(draw(dts_ctree, frequency = "detailed"))
  expect_snapshot_output(draw(dts_ctree, control = draw_control(root = "x", open_ct = "[", close_ct = "]", first_node = "*", next_node = "°", vbranch = "^", hbranch = "->"), frequency = "detailed"))
})
