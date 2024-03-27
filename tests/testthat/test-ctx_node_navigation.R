test_that("parent navigation works", {
  withr::local_seed(1)
  rdts <- sample(1:5, 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 4)
    all_ok <- TRUE
    for (k in 1:(length(rdts) - 2)) {
      for (l in 1:(min(3, length(rdts) - 1 - k))) {
        the_ctx <- rdts[k:(k + l)]
        the_match <- find_sequence(rdts_ctree, the_ctx)
        the_pmatch <- find_sequence(rdts_ctree, the_ctx[-1])
        if (!compare_ctx_node(parent(the_match), the_pmatch)) {
          all_ok <- FALSE
          break
        }
      }
      if (!all_ok) {
        break
      }
    }
    expect_true(all_ok)
  }
})

test_that("children navigation works", {
  withr::local_seed(2)
  rdts <- sample(1:5, 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 4)
    all_ok <- TRUE
    for (k in 1:(length(rdts) - 2)) {
      for (l in 1:(min(3, length(rdts) - 1 - k))) {
        the_ctx <- rdts[(k + 1):(k + l)]
        the_match <- find_sequence(rdts_ctree, the_ctx)
        the_cmatch <- find_sequence(rdts_ctree, rdts[k:(k + l)])
        the_children <- children(the_match)
        if (!length(the_children) == 5 ||
          !compare_ctx_node(the_children[[rdts[k]]], the_cmatch)) {
          all_ok <- FALSE
          break
        }
      }
      if (!all_ok) {
        break
      }
    }
    expect_true(all_ok)
  }
})
