test_that("context objects are printed as expected", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
  expect_snapshot(print(find_sequence(rdts_ctree, c(0, 0))))
  expect_snapshot(print(find_sequence(rdts_ctree, c(1, 0))))
  expect_snapshot(print(rev(find_sequence(rdts_ctree, c(0, 0)))))
  expect_snapshot(print(rev(find_sequence(rdts_ctree, c(1, 0)))))
})

test_that("automatic C++ representation restoration works", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
  a_sequence <- find_sequence(rdts_ctree, c(0, 0))
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

test_that("children are computed correctly at the end of the time series", {
  ## regression test: subscript out of bounds in node_children
  dts <- c(0, 1, 1, 0, 1)
  dts_ctree <- ctx_tree(dts, min_size = 1, backend = "C++")
  expect_no_warning(the_children <- children(find_sequence(dts_ctree, dts[1:3])))
  expect_length(the_children, 0)
  withr::local_seed(3)
  for (k in 1:10) {
    dts <- sample(c(0, 1, 2), sample(5:40, 1), replace = TRUE)
    dts_ctree <- ctx_tree(dts, min_size = 1, backend = "C++")
    dts_rtree <- ctx_tree(dts, min_size = 1, backend = "R")
    for (ctx in contexts(dts_rtree)) {
      the_seq <- as_sequence(ctx)
      expect_no_warning(
        cpp_children <- children(find_sequence(dts_ctree, the_seq))
      )
      r_children <- children(find_sequence(dts_rtree, the_seq))
      expect_identical(names(cpp_children), names(r_children))
    }
  }
})

test_that("positions are reported after serialization", {
  withr::local_seed(4)
  dts <- sample(c(0, 1, 2), 300, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 3, max_depth = 4, backend = "C++")
  ctx_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(dts_ctree, ctx_path)
  restored_ctree <- readRDS(ctx_path)
  expect_identical(
    positions(find_sequence(restored_ctree, c(0, 1))),
    positions(find_sequence(dts_ctree, c(0, 1)))
  )
  model <- vlmc(dts, min_size = 3, max_depth = 4, keep_match = TRUE, backend = "C++")
  a_sequence <- find_sequence(model, c(0))
  saveRDS(a_sequence, ctx_path)
  restored_seq <- readRDS(ctx_path)
  expect_identical(positions(restored_seq), positions(a_sequence))
  expect_error(
    positions(find_sequence(trim(model), c(0))),
    "Cannot report positions if they were not saved"
  )
})

test_that("sequences in the middle of edges are handled correctly", {
  withr::local_seed(5)
  configs <- list(c(2, 6), c(5, 6), c(1, 10), c(3, 3))
  for (config in configs) {
    dts <- sample(c(0, 1, 2), 200, replace = TRUE)
    dts_ctree <- ctx_tree(dts,
      min_size = config[1], max_depth = config[2],
      keep_position = FALSE, backend = "C++"
    )
    dts_rtree <- ctx_tree(dts,
      min_size = config[1], max_depth = config[2],
      backend = "R"
    )
    r_ctxs <- contexts(dts_rtree)
    cpp_nodes <- lapply(r_ctxs, function(ctx) {
      find_sequence(dts_ctree, as_sequence(ctx))
    })
    expect_identical(
      sapply(cpp_nodes, is_context),
      sapply(r_ctxs, is_context)
    )
    expect_equal(
      lapply(cpp_nodes, counts, local = TRUE),
      lapply(r_ctxs, counts, local = TRUE)
    )
    expect_true(all(sapply(contexts(dts_ctree), is_context)))
  }
})
