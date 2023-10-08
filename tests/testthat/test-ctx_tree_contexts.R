test_that("contexts keep the original type of the dts", {
  dts <- sample(c(0L, 1L), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(as_sequence(dts_contexts[[1]]), "integer")
  dts <- sample(c(0, 1), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(as_sequence(dts_contexts[[1]]), "double")
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(as_sequence(dts_contexts[[1]]), "character")
  dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_s3_class(as_sequence(dts_contexts[[1]]), "factor", exact = TRUE)
  expect_equal(levels(as_sequence(dts_contexts[[1]])), levels(dts))
})

test_that("the context tree construction finds the specified contexts", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  ctxs <- list(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  expect_identical(
    lapply(contexts(dts_ctree, reverse = TRUE), as_sequence),
    ctxs
  )
})

test_that("contexts return the appropriate data format", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  expect_snapshot(contexts(dts_tree))
})

test_that("contexts print as expected", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  expect_type(contexts(dts_tree), "list")
  expect_s3_class(contexts(dts_tree, sequence = TRUE), "data.frame")
})

test_that("contexts reversing reverses", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  def_ctx <- contexts(dts_tree)
  rev_ctx <- contexts(dts_tree, reverse = FALSE)
  expect_length(rev_ctx, length(def_ctx))
  for (k in seq_along(def_ctx)) {
    expect_identical(as_sequence(rev_ctx[[k]]), rev(as_sequence(def_ctx[[k]])))
  }
  ## data frame case
  def_ctx <- contexts(dts_tree, frequency = "detailed")
  rev_ctx <- contexts(dts_tree, frequency = "detailed", reverse = FALSE)
  expect_equal(dim(rev_ctx), dim(def_ctx))
  for (k in 1:nrow(def_ctx)) {
    expect_identical(rev_ctx$context[[k]], rev(def_ctx$context[[k]]))
  }
  expect_identical(rev_ctx[-1], def_ctx[-1])
})

test_that("context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  raw_ctx <- contexts(dts_tree, sequence = TRUE)
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(dts_tree, frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  full_ctx <- contexts(dts_tree, frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
})

test_that("basic count consistency", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  full_ctx <- contexts(dts_tree, frequency = "detailed")
  expect_equal(nrow(full_ctx), context_number(dts_tree))
  totals <- apply(full_ctx[3:5], 1, sum)
  expect_equal(full_ctx$freq, totals)
})

test_that("contexts positions are valid", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, keep_position = TRUE)
  full_ctx <- contexts(dts_tree, frequency = "detailed", positions = TRUE)
  ## check first the consistency (we remove matches at the end of the dts)
  expect_equal(
    sapply(full_ctx[["positions"]], \(x) length(x[x != length(dts)])),
    full_ctx$freq
  )
  ## then we check all the matches
  all_valid <- TRUE
  for (k in 1:nrow(full_ctx)) {
    positions <- full_ctx[["positions"]][[k]]
    ctx <- rev(full_ctx[["context"]][[k]])
    for (pos in positions) {
      for (l in seq_along(ctx)) {
        if (ctx[l] != dts[pos - length(ctx) + l]) {
          all_valid <- FALSE
          break
        }
      }
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
