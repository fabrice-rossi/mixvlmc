test_that("trimming removes only what it should remove", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
    dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
    dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
    m_cov <- covlmc(dts, dts_cov, min_size = 10, keep_data = TRUE)
    t_m_cov_model <- trim(m_cov, keep_model = TRUE)
    t_m_cov <- trim(m_cov)
    ## size reduction
    expect_true(object.size(t_m_cov_model) < object.size(m_cov))
    expect_true(object.size(t_m_cov) < object.size(t_m_cov_model))
    ## identical contexts
    expect_identical(
      contexts(m_cov, hsize = TRUE, model = "coef", frequency = "detailed", metrics = TRUE, merging = TRUE),
      contexts(t_m_cov_model, hsize = TRUE, model = "coef", frequency = "detailed", metrics = TRUE, merging = TRUE)
    )
    expect_identical(
      contexts(m_cov, hsize = TRUE, model = "coef", frequency = "detailed", metrics = TRUE, merging = TRUE),
      contexts(t_m_cov, hsize = TRUE, model = "coef", frequency = "detailed", metrics = TRUE, merging = TRUE)
    )
    ## still usable with keep_mode==TRUE
    expect_equal(
      loglikelihood(m_cov, dts, dts_cov),
      loglikelihood(t_m_cov_model, dts, dts_cov)
    )
    expect_no_error(contexts(t_m_cov_model, hsize = TRUE, model = "full"))
    expect_equal(
      simulate(m_cov, nsim = 50, seed = 0, dts_cov),
      simulate(t_m_cov_model, nsim = 50, seed = 0, dts_cov)
    )
    ## errors
    expect_error(
      contexts(t_m_cov, hsize = TRUE, model = "full"),
      "Full model extraction is not supported by fully trimmed covlmc"
    )
    expect_error(
      loglikelihood(t_m_cov, dts, dts_cov),
      "loglikelihood calculation for new data is not supported by fully trimmed covlmc"
    )
    expect_error(
      simulate(t_m_cov, nsim = 50, seed = 0, dts_cov),
      "simulate is not supported by fully trimmed covlmc"
    )
    expect_error(
      metrics(t_m_cov),
      "metrics is not supported by trimmed covlmc"
    )
    expect_error(
      metrics(t_m_cov_model),
      "metrics is not supported by trimmed covlmc"
    )
    expect_error(
      prune(t_m_cov),
      "covlmc must be called with keep_data=TRUE to enable post pruning"
    )
    expect_error(
      prune(t_m_cov_model),
      "covlmc must be called with keep_data=TRUE to enable post pruning"
    )
  }
})

test_that("trimmed model can be drawn", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
    dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
    dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
    m_cov <- covlmc(dts, dts_cov, min_size = 10, keep_data = TRUE)
    t_m_cov_model <- trim(m_cov, keep_model = TRUE)
    t_m_cov <- trim(m_cov)
    expect_snapshot(draw(t_m_cov_model, model = "coef", time_sep = " | ", with_state = TRUE, digits = 2))
    expect_snapshot(draw(t_m_cov, model = "coef", time_sep = " | ", with_state = TRUE, digits = 2))
    expect_snapshot(draw(t_m_cov_model, model = "full", time_sep = " | ", with_state = TRUE, digits = 2))
    expect_snapshot(draw(t_m_cov, model = "full", time_sep = " | ", with_state = TRUE, digits = 2))
  }
})
