test_that("the control list can still be given as the second argument of draw()", {
  withr::local_options(mixvlmc.charset = "ascii")
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 200, replace = TRUE)
  ctrl <- draw_control(charset = charset_ascii(root = "R"))
  for (backend in c("R", "C++")) {
    ct <- ctx_tree(x, max_depth = 3, backend = backend)
    m <- vlmc(x, backend = backend)
    for (model in list(ct, m)) {
      lifecycle::expect_deprecated(capture.output(draw(model, ctrl)))
      withr::with_options(list(lifecycle_verbosity = "quiet"), {
        expect_identical(
          capture.output(draw(model, ctrl)),
          capture.output(draw(model, control = ctrl))
        )
      })
    }
  }
})

test_that("the digits and time_sep arguments of draw.covlmc() are deprecated", {
  withr::local_options(mixvlmc.charset = "ascii")
  pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
  lifecycle::expect_deprecated(capture.output(draw(m_cov, digits = 2)))
  lifecycle::expect_deprecated(capture.output(draw(m_cov, model = "full", time_sep = " % ")))
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    capture.output(draw(m_cov, digits = 2)),
    capture.output(draw(m_cov, control = draw_control(digits = 2)))
  )
  expect_identical(
    capture.output(draw(m_cov, model = "full", time_sep = " % ")),
    capture.output(draw(m_cov,
      model = "full",
      control = draw_control(charset = charset_ascii(time_sep = " % "))
    ))
  )
  expect_identical(
    capture.output(draw(m_cov, draw_control(digits = 2))),
    capture.output(draw(m_cov, control = draw_control(digits = 2)))
  )
})

test_that("draw() reports unused arguments", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 200, replace = TRUE)
  for (backend in c("R", "C++")) {
    expect_error(draw(ctx_tree(x, backend = backend), tab_orientation = "h"))
    expect_error(draw(vlmc(x, backend = backend), probs = TRUE))
  }
  cov <- data.frame(y = stats::runif(200))
  m_cov <- covlmc(x, cov)
  expect_error(draw(m_cov, tab_orientation = "horizontal"), "tab_orientation")
  expect_error(draw(m_cov, "text", draw_control(), "full", FALSE, FALSE, TRUE, 3), "unnamed")
  ## deprecated arguments are still supported
  lifecycle::expect_deprecated(draw(m_cov, digits = 2))
})
