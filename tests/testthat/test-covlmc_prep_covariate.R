test_that("prepare_covariate is called with the proper parameters", {
  skip_on_ci()
  skip_on_cran()
  d_model <- create_demo_covlmc()
  nx <- to_dts(d_model$rdts, d_model$model$vals)
  x <- nx$ix + 1
  ctx <- c()
  all_true <- TRUE
  for (i in 1:length(x)) {
    subtree <- match_context_co(d_model$model, ctx)
    if (subtree$merged) {
      local_model <- subtree$tree$merged_model
    } else if (is.null(subtree$tree[["model"]])) {
      local_model <- subtree$tree$extended_model
    } else {
      local_model <- subtree$tree$model
    }
    mm <- prepare_covariate(d_model$cov, i - subtree$depth - 1,
      d = local_model$hsize,
      from = subtree$depth - local_model$hsize
    )
    ## the context of i has been found. It's length is subtree$depth. We expect
    ## the covariates to be in positions i-local_model$hsize to i-1

    if (local_model$hsize == 0) {
      all_true <- ncol(mm) == 0L
    } else {
      expected_cov <- d_model$cov[(i - local_model$hsize):(i - 1), , drop = FALSE]
      expected_cov <- rev(expected_cov[[1]])
      the_mm <- unlist(mm)
      names(the_mm) <- NULL
      all_true <- identical(the_mm, expected_cov)
    }
    if (!all_true) {
      break
    }
    if (i <= length(x)) {
      j <- max(i - depth(d_model$model) + 1, 1)
      ctx <- x[i:j]
    }
  }
  expect_true(all_true)
})

test_that("prepare_covariate is called with the proper parameters (real data)", {
  skip_on_ci()
  skip_on_cran()
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.1)
  nx <- to_dts(rdts, m_cov$vals)
  x <- nx$ix + 1
  ctx <- c()
  all_true <- TRUE
  for (i in 1:length(x)) {
    subtree <- match_context_co(m_cov, ctx)
    if (subtree$merged) {
      local_model <- subtree$tree$merged_model
    } else if (is.null(subtree$tree[["model"]])) {
      local_model <- subtree$tree$extended_model
    } else {
      local_model <- subtree$tree$model
    }
    mm <- prepare_covariate(rdts_cov, i - subtree$depth - 1,
      d = local_model$hsize,
      from = subtree$depth - local_model$hsize
    )
    ## the context of i has been found. It's length is subtree$depth. We expect
    ## the covariates to be in positions i-local_model$hsize to i-1

    if (local_model$hsize == 0) {
      all_true <- ncol(mm) == 0L
    } else {
      expected_cov <- rdts_cov[(i - local_model$hsize):(i - 1), , drop = FALSE]
      expected_cov <- rev(expected_cov[[1]])
      the_mm <- unlist(mm)
      names(the_mm) <- NULL
      all_true <- identical(the_mm, expected_cov)
    }
    if (!all_true) {
      break
    }
    if (i <= length(x)) {
      j <- max(i - depth(m_cov) + 1, 1)
      ctx <- x[i:j]
    }
  }
  expect_true(all_true)
})
