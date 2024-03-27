test_that("draw obeys its contract (with vgam) LaTeX", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)),
    c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE))
  )
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y,
    sample(c("A", "B", "C"), length(rdts), replace = TRUE)
  ))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.01, min_size = 1.5)
  expect_snapshot_output(draw(model,
    format = "latex", model = NULL,
    p_value = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex"
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(orientation = "h")
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(tab_orientation = "h")
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(fontsize = "small", decoration = "circle"),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(orientation = "h", prob_fontsize = "scriptsize"),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex", control = draw_control(digits = 2),
    with_state = TRUE,
  ))
})

test_that("draw obeys its contract (with nnet) LaTeX", {
  withr::local_seed(0)
  withr::local_options(mixvlmc.predictive = "multinom")
  rdts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)),
    c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE))
  )
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y,
    sample(c("A", "B", "C"), length(rdts), replace = TRUE)
  ))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.01, min_size = 1.5)
  expect_snapshot_output(draw(model,
    format = "latex", model = NULL,
    p_value = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(digits = 2)
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(
      orientation = "h",
      digits = 1
    )
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(
      tab_orientation = "h",
      digits = 2
    )
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(
      fontsize = "small",
      decoration = "circle",
      digits = 3
    ),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex",
    control = draw_control(
      orientation = "h",
      prob_fontsize = "scriptsize",
      digits = 2
    ),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    format = "latex", control = draw_control(digits = 2),
    with_state = TRUE,
  ))
})

test_that("draw handles cases when levels have been dropped LaTeX", {
  withr::local_seed(0)
  x <- sample(c(0, 1), 200, replace = TRUE)
  xl1 <- forward_match_all_ctx_counts(x, 2)
  xl2_0 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[1]])
  xl2_1 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[2]])
  y <- rep(1, length(x))
  y[xl2_0$positions[[1]] + 1] <-
    sample(2:4, length(xl2_0$positions[[1]]), replace = TRUE)
  y[xl2_0$positions[[2]] + 1] <-
    sample(c(1, 3:4), length(xl2_0$positions[[2]]), replace = TRUE)
  y[xl2_1$positions[[1]] + 1] <-
    sample(c(1:2, 4), length(xl2_1$positions[[1]]), replace = TRUE)
  y[xl2_1$positions[[2]] + 1] <-
    sample(1:3, length(xl2_1$positions[[2]]), replace = TRUE)
  y <- as.factor(y)
  z <- runif(length(x)) + c(x[-1], 0) / 4
  rdts_cov <- data.frame(y = y, z = z)
  m_cov <- covlmc(x = x, covariate = rdts_cov, min_size = 3, alpha = 0.5)
  expect_snapshot_output(draw(m_cov,
    format = "latex",
    control = draw_control(digits = 1)
  ))
  expect_snapshot_output(draw(m_cov,
    format = "latex",
    control = draw_control(orientation = "h", digits = 2),
    with_state = TRUE
  ))
})

test_that("draw handles cases when multinom is used for two states time series LaTeX", {
  withr::local_seed(0)
  withr::local_options(mixvlmc.predictive = "multinom")
  x <- sample(c(0, 1), 200, replace = TRUE)
  xl1 <- forward_match_all_ctx_counts(x, 2)
  xl2_0 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[1]])
  xl2_1 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[2]])
  y <- rep(1, length(x))
  y[xl2_0$positions[[1]] + 1] <- sample(2:4, length(xl2_0$positions[[1]]), replace = TRUE)
  y[xl2_0$positions[[2]] + 1] <- sample(c(1, 3:4), length(xl2_0$positions[[2]]), replace = TRUE)
  y[xl2_1$positions[[1]] + 1] <- sample(c(1:2, 4), length(xl2_1$positions[[1]]), replace = TRUE)
  y[xl2_1$positions[[2]] + 1] <- sample(1:3, length(xl2_1$positions[[2]]), replace = TRUE)
  y <- as.factor(y)
  z <- runif(length(x)) + c(x[-1], 0) / 4
  rdts_cov <- data.frame(y = y, z = z)
  m_cov <- covlmc(x = x, covariate = rdts_cov, min_size = 3, alpha = 0.5)
  expect_snapshot_output(draw(m_cov,
    format = "latex",
    control = draw_control(tab_orientation = "v", digits = 1)
  ))
  expect_snapshot_output(draw(m_cov,
    format = "latex",
    control = draw_control(digits = 1, fontsize = "small"),
    with_state = TRUE
  ))
})

test_that("draw handles degenerate cases LaTeX", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
    rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
    rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
    m_cov <- covlmc(rdts, rdts_cov, min_size = 10, keep_data = TRUE)
    expect_snapshot_output(draw(m_cov,
      format = "latex",
      control = draw_control(digits = 2),
      with_state = TRUE
    ))
    expect_snapshot_output(draw(m_cov,
      format = "latex",
      control = draw_control(prob_fontsize = "tiny", digits = 2),
      with_state = TRUE
    ))
  }
})
