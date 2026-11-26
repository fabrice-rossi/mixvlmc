test_that("loglikelihood computes the expected values", {
  skip_on_cran()
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y)
      sll <- co_slow_loglikelihood(x_covlmc, x, initial = initial, newcov = df_y)
      dll <- loglikelihood(x_covlmc, initial = initial)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "df"), attr(sll, "df"))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
      expect_equal(as.numeric(dll), as.numeric(sll))
      expect_equal(attr(dll, "df"), attr(sll, "df"))
      expect_equal(attr(dll, "nobs"), attr(sll, "nobs"))
    }
  }
})

test_that("loglikelihood handles constant models", {
  x <- rep(c(0, 1), 1000)
  y <- data.frame(y = rep(0, length(x)))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, y)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = y)
      sll <- co_slow_loglikelihood(x_covlmc, x, initial = initial, newcov = y)
      dll <- loglikelihood(x_covlmc, initial = initial)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "df"), attr(sll, "df"))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
      expect_equal(as.numeric(dll), as.numeric(sll))
      expect_equal(attr(dll, "df"), attr(sll, "df"))
      expect_equal(attr(dll, "nobs"), attr(sll, "nobs"))
    }
  }
})


test_that("loglikelihood computes the expected values with ignore", {
  skip_on_cran()
  withr::local_seed(42)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific", "extended")) {
      to_ignore <- depth(x_covlmc) + sample(1:50, 1)
      fll <- loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y, ignore = to_ignore)
      sll <- co_slow_loglikelihood(x_covlmc, x, initial = initial, newcov = df_y, ignore = to_ignore)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "df"), attr(sll, "df"))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
      expect_error(loglikelihood(x_covlmc, initial = initial, ignore = to_ignore))
      if (initial != "truncated") {
        fll <- loglikelihood(x_covlmc, initial = initial, ignore = 1)
        sll <- co_slow_loglikelihood(x_covlmc, x, initial = initial, newcov = df_y, ignore = 1)
        expect_equal(as.numeric(fll), as.numeric(sll))
        expect_equal(attr(fll, "df"), attr(sll, "df"))
        expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
      }
    }
  }
})

test_that("the likelihood calculation is valid", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific", "extended")) {
      expect_equal(
        as.numeric(logLik(x_covlmc, initial = initial)),
        as.numeric(loglikelihood(x_covlmc, initial = initial))
      )
      expect_equal(
        loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y),
        loglikelihood(x_covlmc, initial = initial)
      )
    }
  }
})

test_that("likelihood calculation on real data", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.1)
  for (initial in c("truncated", "specific", "extended")) {
    fll <- loglikelihood(m_cov, initial = initial, newdata = rdts, newcov = rdts_cov)
    expect_identical(
      fll,
      loglikelihood(m_cov, initial = initial),
      tolerance = 1e-7
    )
    sll <- co_slow_loglikelihood(m_cov, rdts, initial = initial, newcov = rdts_cov)
    expect_equal(as.numeric(fll), as.numeric(sll))
    expect_equal(attr(fll, "df"), attr(sll, "df"))
    expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
  }
})

test_that("likelihood calculation on real data with merged models", {
  skip_on_cran()
  skip_on_ci()
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    d_model <- build_degenerate_elec_model(FALSE)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(d_model$model,
        newdata = d_model$rdts,
        newcov = d_model$cov,
        initial = initial
      )
      expect_identical(fll,
        loglikelihood(d_model$model, initial = initial),
        tolerance = 1e-7
      )
      sll <- co_slow_loglikelihood(d_model$model,
        newdata = d_model$rdts,
        initial = initial,
        newcov = d_model$cov
      )
      expect_equal(as.numeric(fll), as.numeric(sll))
    }
  }
})

test_that("likelihood calculation artifical data with merged models", {
  skip_on_cran()
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    d_model <- create_demo_covlmc()
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(d_model$model,
        newdata = d_model$rdts,
        newcov = d_model$cov,
        initial = initial
      )
      expect_identical(fll,
        loglikelihood(d_model$model, initial = initial),
        tolerance = 1e-7
      )
      sll <- co_slow_loglikelihood(d_model$model,
        newdata = d_model$rdts,
        initial = initial,
        newcov = d_model$cov
      )
      expect_equal(as.numeric(fll), as.numeric(sll))
    }
  }
})

test_that("loglikelihood rejects missing values in new covariates", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 5)
  new_cov <- dts_cov[1:50, , drop = FALSE]
  new_cov$day_night[20] <- NA
  expect_error(loglikelihood(m_cov, newdata = dts[1:50], newcov = new_cov), "missing values")
})

test_that("the likelihood of a depth 0 covlmc uses all the observations", {
  x <- c(1, rep(0, 9))
  x_cov <- data.frame(z = seq_along(x))
  x_vlmc <- vlmc(x, min_size = 100, backend = "R")
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(x, x_cov, min_size = 100)
    expect_equal(depth(m_cov), 0)
    m_ll <- loglikelihood(m_cov)
    expect_equal(as.numeric(m_ll), as.numeric(loglikelihood(x_vlmc)), tolerance = 1e-5)
    expect_equal(attr(m_ll, "nobs"), length(x))
    expect_equal(loglikelihood(m_cov, newdata = x, newcov = x_cov), m_ll)
    ## shorter new data
    m_ll_new <- loglikelihood(m_cov, newdata = x[1:5], newcov = x_cov[1:5, , drop = FALSE])
    expect_equal(as.numeric(m_ll_new), as.numeric(loglikelihood(x_vlmc, newdata = x[1:5])),
      tolerance = 1e-5
    )
    expect_equal(attr(m_ll_new, "nobs"), 5)
    ## ignoring the first observation
    m_ll_ign <- loglikelihood(m_cov,
      newdata = x, newcov = x_cov,
      initial = "specific", ignore = 1
    )
    expect_equal(as.numeric(m_ll_ign),
      as.numeric(loglikelihood(x_vlmc, newdata = x, initial = "specific", ignore = 1)),
      tolerance = 1e-5
    )
  }
})

test_that("loglikelihood ignores impossible transitions in the ignored observations", {
  withr::local_seed(0)
  n <- 500
  ## a is never followed by a
  x <- character(n)
  x[1] <- "b"
  for (i in 2:n) {
    if (x[i - 1] == "a") {
      x[i] <- sample(c("b", "c"), 1)
    } else {
      x[i] <- sample(c("a", "b", "c"), 1)
    }
  }
  x_cov <- data.frame(z = rnorm(n))
  new_x <- c("a", "a", "b", x[4:100])
  new_cov <- x_cov[1:100, , drop = FALSE]
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(x, x_cov, min_size = 5, alpha = 0.5)
    for (initial in c("truncated", "specific", "extended")) {
      m_ll <- loglikelihood(m_cov,
        newdata = new_x, newcov = new_cov,
        initial = initial, ignore = 2
      )
      expect_true(is.finite(m_ll))
      expect_equal(
        m_ll,
        co_slow_loglikelihood(m_cov, new_x, initial = initial, ignore = 2, newcov = new_cov),
        ignore_attr = TRUE
      )
    }
    ## not ignored
    expect_equal(
      as.numeric(loglikelihood(m_cov, newdata = new_x, newcov = new_cov, initial = "extended")),
      -Inf
    )
  }
})

test_that("the number of parameters counts the coefficients estimated by the engines", {
  ## the covariate can take the value "c" only after state 0: this level is
  ## absent from the data of the contexts that do not start with 0. State 2
  ## never follows state 1: it is absent from the data of the contexts that
  ## start with 1
  withr::local_seed(0)
  n <- 2000
  x <- integer(n)
  z <- character(n)
  for (t in 1:n) {
    z[t] <- if (x[t] == 0L) sample(c("a", "b", "c"), 1) else sample(c("a", "b"), 1)
    if (t < n) {
      p <- switch(z[t],
        a = c(0.7, 0.2, 0.1),
        b = c(0.1, 0.7, 0.2),
        c = c(0.2, 0.1, 0.7)
      )
      if (x[t] == 1L) {
        p[3] <- 0
      }
      x[t + 1] <- sample(0:2, 1, prob = p)
    }
  }
  cov <- data.frame(z = factor(z))
  ## parameters of a model: one intercept per state but one, and one parameter
  ## per covariate column of the design matrix (without the unused levels) and
  ## per observed state but one
  model_df <- function(model) {
    target <- model$data$target
    mm <- droplevels(model$data$local_mm)
    k <- length(unique(target))
    if (ncol(mm) == 0) {
      q <- 1
    } else {
      q <- ncol(stats::model.matrix(~., data = mm))
    }
    2 + (k - 1) * (q - 1)
  }
  all_models <- function(model, with_extended) {
    rec <- function(ct, d) {
      res <- list()
      for (f in c("model", "merged_model")) {
        if (!is.null(ct[[f]])) {
          res <- c(res, list(ct[[f]]))
        }
      }
      if (with_extended && d < depth(model) && !is.null(ct$extended_model)) {
        res <- c(res, list(ct$extended_model))
      }
      for (child in ct$children) {
        if (length(child) > 0) {
          res <- c(res, rec(child, d + 1))
        }
      }
      res
    }
    rec(model, 0)
  }
  dfs <- list()
  for (engine in c("glm", "multinom")) {
    withr::with_options(list(mixvlmc.predictive = engine), {
      model <- covlmc(x, cov, min_size = 5)
      expect_gt(covariate_depth(model), 0)
      models <- all_models(model, FALSE)
      ## the data exercise the absent states and the unused levels
      expect_true(any(sapply(models, function(m) {
        length(unique(m$data$target)) < 3
      })))
      expect_true(any(sapply(models, function(m) {
        any(sapply(m$data$local_mm, function(v) nlevels(droplevels(v)) < 3))
      })))
      expect_equal(attr(logLik(model), "df"), sum(sapply(models, model_df)))
      expect_equal(
        attr(logLik(model, initial = "extended"), "df"),
        sum(sapply(all_models(model, TRUE), model_df))
      )
      dfs[[engine]] <- attr(logLik(model), "df")
    })
  }
  ## the engines agree
  expect_equal(dfs[["glm"]], dfs[["multinom"]])
})
