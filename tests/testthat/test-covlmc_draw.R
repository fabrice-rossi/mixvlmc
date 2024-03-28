test_that("draw obeys its contract (with vgam)", {
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
  for (charset in c("ascii", "utf8")) {
    withr::local_options(mixvlmc.charset = charset)
    expect_snapshot_output(draw(model, model = NULL, p_value = TRUE))
    expect_snapshot_output(draw(prune(model, 0.0001)))
  }
  expect_snapshot_output(draw(prune(model, 0.0001),
    control = draw_control(charset = charset_ascii(time_sep = " % "))
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    model = "full",
    control = draw_control(charset = charset_ascii(time_sep = " % "))
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    model = "full",
    control = draw_control(charset = charset_ascii(time_sep = " % ")),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    model = "full",
    control = draw_control(charset = charset_ascii(
      time_sep = " % ",
      intercept_sep = " + "
    )),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    model = "coef",
    control = draw_control(charset = charset_ascii(time_sep = " % ")),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(prune(model, 0.0001),
    model = "coef", control = draw_control(charset = charset_ascii(
      time_sep = " % ",
      level_sep = " @ "
    )),
    with_state = TRUE,
  ))
  ## the following snapshots depend on blas version
  ## skip_on_ci()
  ##  expect_snapshot_output(draw(model))
  ##  expect_snapshot_output(draw(model, control = draw_control(charset = charset_ascii(time_sep = " % "))))
  ##  expect_snapshot_output(draw(model, control = draw_control(digits = 3)))
  ##  expect_snapshot_output(draw(model, model = NULL, control = draw_control(digits = 2)))
  ##  expect_snapshot_output(draw(model, p_value = FALSE, control = draw_control(digits = 1)))
  ##  expect_snapshot_output(draw(model, model = "full", control = draw_control(charset = charset_ascii(time_sep = " ~ "), digits = 1)))
  ##  expect_snapshot_output(draw(model, model = "full", control = draw_control(charset = charset_ascii(time_sep = " ~ "), digits = 5), with_state = TRUE))
  ##  expect_snapshot_output(draw(model, model = "coef", control = draw_control(charset = charset_ascii(time_sep = " ~ "), digits = 5), with_state = TRUE))
})

test_that("draw obeys its contract (with nnet)", {
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
  expect_snapshot_output(draw(model))
  expect_snapshot_output(draw(model,
    control = draw_control(charset = charset_ascii(time_sep = " % "))
  ))
  expect_snapshot_output(draw(model, control = draw_control(digits = 3)))
  expect_snapshot_output(draw(model,
    model = NULL,
    control = draw_control(digits = 2)
  ))
  expect_snapshot_output(draw(model,
    p_value = FALSE,
    control = draw_control(digits = 1)
  ))
  expect_snapshot_output(draw(model,
    model = "full", time_sep = " ^ ",
    control = draw_control(digits = 1)
  ))
  expect_snapshot_output(draw(model,
    model = "full", time_sep = " ^ ",
    control = draw_control(digits = 3),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(model,
    model = "coef", time_sep = " ^ ",
    control = draw_control(digits = 3),
    with_state = TRUE
  ))
  expect_snapshot_output(draw(model,
    model = "coef", time_sep = " ^ ",
    with_state = TRUE, control = draw_control(digits = 3, charset = charset_ascii(level_sep = " @ "))
  ))
})

test_that("draw handles cases when levels have been dropped", {
  withr::local_seed(0)
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
    model = "full",
    control = draw_control(digits = 1)
  ))
  expect_snapshot_output(draw(m_cov,
    model = "full",
    control = draw_control(charset = charset_ascii(time_sep = " % "), digits = 1)
  ))
  expect_snapshot_output(draw(m_cov,
    model = "full",
    control = draw_control(charset = charset_ascii(time_sep = " % ", intercept_sep = " + "), digits = 2),
    with_state = TRUE
  ))
})

test_that("draw handles cases when multinom is used for two states time series", {
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
    control = draw_control(digits = 1),
    model = "full"
  ))
  expect_snapshot_output(draw(m_cov,
    model = "full",
    control = draw_control(charset = charset_ascii(time_sep = " % "), digits = 1)
  ))
  expect_snapshot_output(draw(m_cov,
    model = "full",
    control = draw_control(
      digits = 1,
      charset = charset_ascii(time_sep = " % ", intercept_sep = " + ")
    ),
    with_state = TRUE
  ))
})

test_that("draw handles degenerate cases", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
    rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
    rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
    m_cov <- covlmc(rdts, rdts_cov, min_size = 10, keep_data = TRUE)
    expect_snapshot_output(draw(m_cov,
      model = "coef",
      with_state = TRUE
    ))
    expect_snapshot_output(draw(m_cov,
      model = "coef",
      control = draw_control(charset = charset_ascii(time_sep = " % "), digits = 2),
      with_state = TRUE
    ))
    expect_snapshot_output(draw(m_cov,
      model = "full",
      control = draw_control(
        charset = charset_ascii(time_sep = " % ", intercept_sep = " + "),
        digits = 2
      ),
      with_state = TRUE
    ))
  }
})

test_that("draw handles constant models", {
  x <- rep(c(0, 1), 1000)
  y <- data.frame(y = rep(0, length(x)))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, y)
    for (format in c("text", "latex")) {
      expect_snapshot_output(draw(x_covlmc, format = format, model = "coef"))
      expect_snapshot_output(draw(x_covlmc, format = format, model = "full"))
    }
  }
})
