test_that("basic find_sequences works", {
  withr::local_seed(0)
  raw_x <- sample(c("A", "B", "C"), 250, replace = TRUE)
  y <- ifelse(runif(length(raw_x)) > 0.5, c(
    raw_x[-1],
    sample(c("A", "B", "C"), 1)
  ),
  c(raw_x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE))
  )
  y <- as.factor(ifelse(runif(length(raw_x)) > 0.2, y,
    sample(c("A", "B", "C"), length(raw_x), replace = TRUE)
  ))
  df_y <- data.frame(y = y)
  x <- dts(raw_x)
  model <- covlmc(x, df_y, alpha = 0.05, min_size = 3 / 2)
  expect_null(parent(find_sequence(model, character())))
  expect_null(parent(find_sequence(model, x[0])))
  expect_equal(positions(find_sequence(model, character())), 0:(length(x) - 1))
  expect_equal(positions(find_sequence(model, x[0])), 0:(length(x) - 1))
  expect_null(find_sequence(model, x[1:(depth(model) + 1)]))
  expect_no_error(find_sequence(model, "A"))
  expect_no_error(find_sequence(model, "B"))
  expect_no_error(find_sequence(model, "C"))
})

test_that("models are correctly reported", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(rdts, df_y, alpha = 0.01, min_size = 3 / 2)
    ctxs_obj <- contexts(model)
    ctxs_df <- contexts(model, type = "data.frame", model = "coef")
    ctxs_coef <- lapply(ctxs_obj, model, type = "coef")
    expect_identical(ctxs_coef, unclass(ctxs_df$coef))
    ctxs_df_f <- contexts(model, type = "data.frame", model = "full")
    ctxs_full <- lapply(ctxs_obj, model, type = "full")
    expect_identical(ctxs_full, unclass(ctxs_df_f$model))
  }
})

test_that("metrics and hsize are correctly reported", {
  withr::local_seed(1)
  rdts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.01, min_size = 3 / 2)
  ctxs_obj <- contexts(model)
  ctxs_df <- contexts(model, type = "data.frame", metrics = TRUE, hsize = TRUE)
  ctxs_metrics <- lapply(ctxs_obj, metrics)
  expect_identical(sapply(ctxs_metrics, \(x) x$accuracy), ctxs_df$accuracy)
  expect_identical(sapply(ctxs_metrics, \(x) x$auc), ctxs_df$auc)
  expect_identical(sapply(ctxs_obj, covariate_memory), ctxs_df$hsize)
})

test_that("merged models are correctly reported", {
  d_model <- build_degenerate_elec_model(TRUE)
  ctxs_obj <- contexts(d_model$model)
  ctxs_df <- contexts(d_model$model, type = "data.frame", model = "full", merging = TRUE)
  ctxs_merged <- sapply(ctxs_obj, is_merged)
  expect_identical(ctxs_merged, ctxs_df$merged)
  for (k in seq_along(ctxs_obj)) {
    if (!is_merged(ctxs_obj[[k]])) {
      expect_null(merged_with(ctxs_obj[[k]]))
    } else {
      mwith <- merged_with(ctxs_obj[[k]])
      expect_equal(length(mwith), length(states(d_model$model)))
      expect_equal(names(mwith), as.character(states(d_model$model)))
      for (j in seq_along(mwith)) {
        if (!is.null(mwith[[j]])) {
          expect_identical(
            model(ctxs_obj[[k]], type = "full"),
            model(mwith[[j]], type = "full")
          )
        }
      }
    }
  }
})

test_that("sequences are properly extracted", {
  d_model <- build_degenerate_elec_model(TRUE)
  ctxs_obj <- contexts(d_model$model)
  for (k in seq_along(ctxs_obj)) {
    direct_seq <- find_sequence(d_model$model, as_sequence(ctxs_obj[[k]]))
    expect_identical(direct_seq, ctxs_obj[[k]])
  }
})

test_that("find_sequence handles correctly contexts not merged", {
  d_model <- build_degenerate_elec_model(FALSE)
  model <- d_model$model
  vals <- states(model)
  ## in this model, the contexts (low, typical) and (high, typical) are merged
  expect_identical(model$children[[2]]$merged, c(1L, 3L))
  expect_true(is_merged(find_sequence(model, vals[c(1, 2)])))
  expect_true(is_merged(find_sequence(model, vals[c(3, 2)])))
  ## we remove (high, typical) from the merged contexts
  model$children[[2]]$merged <- 1L
  expect_true(is_merged(find_sequence(model, vals[c(1, 2)])))
  expect_null(find_sequence(model, vals[c(3, 2)]))
  ## and then (low, typical)
  model$children[[2]]$merged <- 3L
  expect_null(find_sequence(model, vals[c(1, 2)]))
  expect_true(is_merged(find_sequence(model, vals[c(3, 2)])))
})

test_that("metrics reports clearly that a node is not a context", {
  pc <- powerconsumption[powerconsumption$week %in% 6:7, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov)
  node <- find_sequence(m_cov, states(m_cov)[1])
  expect_false(is_context(node))
  expect_error(metrics(node), "not a context")
})

test_that("positions of the empty sequence are 0 based", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 5)
  root <- find_sequence(m_cov, states(m_cov)[0])
  expect_identical(positions(root), 0:(length(dts) - 1L))
  ## positions rebuilt for the empty sequence
  m_cov$match <- NULL
  m_cov$keep_match <- TRUE
  root <- find_sequence(m_cov, states(m_cov)[0])
  expect_identical(positions(root), 0:(length(dts) - 1L))
})
