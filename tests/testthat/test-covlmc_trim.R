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
  }
})
