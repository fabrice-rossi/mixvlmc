test_that("making nodes explicit does not break anything", {
  withr::local_seed(1)
  for (val in 1:4) {
    dts <- sample(0:val, 30, replace = TRUE)
    tree <- build_suffix_tree(dts, val + 1)
    tree$compute_counts(val, TRUE)
    #    dts <- rep(0:3, 4)
    #  tree <- build_suffix_tree(dts, 4)
    #  tree$compute_counts(3, TRUE)
    ## save values before the call to make_explicit
    b_ctx <- tree$contexts(1, -1)
    b_rep <- tree$representation()
    b_pos <- vector(mode = "list", length(b_ctx))
    for (k in seq_along(b_ctx)) {
      b_pos[[k]] <- tree$positions(b_ctx[[k]])
    }
    tree$make_explicit()
    ## values after the call to make_explicit
    a_ctx <- tree$contexts(1, -1)
    a_rep <- tree$representation()
    a_pos <- vector(mode = "list", length(a_ctx))
    for (k in seq_along(a_ctx)) {
      a_pos[[k]] <- tree$positions(a_ctx[[k]])
    }
    expect_length(a_ctx, length(b_ctx))
    ctx_ok <- TRUE
    pos_ok <- TRUE
    for (l in seq_along(a_ctx)) {
      find_a <- Position(\(x) identical(x, a_ctx[[l]]), b_ctx, nomatch = 0)
      if (find_a == 0) {
        ctx_ok <- FALSE
        pos_ok <- FALSE
        break
      } else {
        pos_ok <- identical(a_pos[[l]], b_pos[[find_a]])
        if (!pos_ok) {
          break
        }
      }
    }
    expect_true(ctx_ok)
    expect_true(pos_ok)
  }
})
