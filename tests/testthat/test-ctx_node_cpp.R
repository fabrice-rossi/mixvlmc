test_that("context objects are printed as expected", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3, backend = "C++")
  expect_snapshot(print(find_sequence(dts_ctree, c(0, 0))))
  expect_snapshot(print(find_sequence(dts_ctree, c(1, 0))))
  expect_snapshot(print(rev(find_sequence(dts_ctree, c(0, 0)))))
  expect_snapshot(print(rev(find_sequence(dts_ctree, c(1, 0)))))
})

test_that("automatic C++ representation restoration works", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3, backend = "C++")
  a_sequence <- find_sequence(dts_ctree, c(0, 0))
  expect_false(extptr_is_null(a_sequence$node_env$node))
  seq_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(a_sequence, seq_path)
  restored_seq <- readRDS(seq_path)
  expect_true(extptr_is_null(restored_seq$tree$root$.pointer))
  expect_true(extptr_is_null(restored_seq$node_env$node))
  expect_true(identical(counts(a_sequence), counts(restored_seq)))
  expect_false(extptr_is_null(restored_seq$node_env$node))
})

test_that("find_sequence handles sequences longer than the end of the time series", {
  ## regression test: subscript out of bounds in raw_find_sequence
  dts <- c(0, 1, 1, 0, 1)
  dts_ctree <- ctx_tree(dts, min_size = 1, backend = "C++")
  dts_rtree <- ctx_tree(dts, min_size = 1, backend = "R")
  for (l in 1:6) {
    all_seqs <- as.matrix(expand.grid(rep(list(c(0, 1)), l)))
    for (k in seq_len(nrow(all_seqs))) {
      the_seq <- unname(all_seqs[k, ])
      expect_no_warning(cpp_seq <- find_sequence(dts_ctree, the_seq))
      expect_identical(is.null(cpp_seq), is.null(find_sequence(dts_rtree, the_seq)))
    }
  }
})
