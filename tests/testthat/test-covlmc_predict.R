test_that("covlmc predict returns the same value(s) for zero depth model", {
  for (k in 1:2) {
    data_set <- build_data_set(100, seed = k)
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- covlmc(data_set$x, data_set$covariate, alpha = 0.0001)
      ## make sure we are in the constant model case
      expect_equal(context_number(model), 1L)
      model_pred <- predict(model, data_set$x, data_set$covariate)
      expect_equal(
        model_pred,
        rep(model_pred[1], length(data_set$x) + 1)
      )
      model_pred <- predict(model, data_set$x, data_set$covariate, type = "probs")
      dimnames(model_pred) <- NULL
      expect_equal(
        model_pred,
        matrix(model_pred[1, ],
          nrow = length(data_set$x) + 1, ncol = ncol(model_pred),
          byrow = TRUE
        )
      )
    }
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      data_set <- build_data_set_3_model(250, seed = k, alpha = 1e-9)
      model <- data_set$model
      ## make sure we are in the constant model case
      if (context_number(model) != 1L) {
        co <- cutoff(model)
        model <- prune(model, min(co))
      }
      expect_equal(context_number(model), 1L)
      model_pred <- predict(model, data_set$rdts, data_set$cov)
      expect_equal(
        model_pred,
        rep(model_pred[1], length(data_set$rdts) + 1)
      )
      model_pred <- predict(model, data_set$rdts, data_set$cov, type = "probs")
      dimnames(model_pred) <- NULL
      expect_equal(
        model_pred,
        matrix(model_pred[1, ],
          nrow = length(data_set$rdts) + 1, ncol = ncol(model_pred),
          byrow = TRUE
        )
      )
    }
  }
})

test_that("covlmc predict returns deterministic results", {
  for (k in 1:2) {
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      data_set <- build_data_set_3_model(250, seed = 0, alpha = 0.1)
      model <- data_set$model
      ## make sure we are in the constant model case
      model_pred <- predict(model, data_set$rdts, data_set$cov, type = "probs")
      model_pred_2 <- predict(model, data_set$rdts, data_set$cov, type = "probs")
      expect_equal(
        model_pred,
        model_pred_2
      )
    }
  }
})

test_that("covlmc predict returns probabilities", {
  for (k in 1:2) {
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      data_set <- build_data_set_3_model(250, seed = 0, alpha = 0.1)
      model <- data_set$model
      ## make sure we are in the constant model case
      model_pred <- predict(model, data_set$rdts, data_set$cov, type = "probs")
      expect_probabilities(model_pred)
      data_set <- build_degenerate_elec_model()
      model <- data_set$model
      ## make sure we are in the constant model case
      model_pred <- predict(model, data_set$rdts, data_set$cov, type = "probs")
      expect_probabilities(model_pred)
    }
  }
})

test_that("the semantics of final_pred is respected", {
  pc_week_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
  elec <- pc_week_15_16$active_power
  elec_rdts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)

    elec_tune <- tune_covlmc(elec_rdts, elec_cov, min_size = 5)
    elec_model <- as_covlmc(elec_tune)
    pred_w_final <- predict(elec_model, elec_rdts[1:500], elec_cov[1:500, , drop = FALSE],
      final_pred = TRUE
    )
    pred_wo_final <- predict(elec_model, elec_rdts[1:500], elec_cov[1:500, , drop = FALSE],
      final_pred = FALSE
    )
    expect_length(
      pred_w_final,
      500 + 1
    )
    expect_length(
      pred_wo_final,
      500
    )
    expect_identical(
      pred_wo_final,
      pred_w_final[-length(pred_w_final)]
    )
    probs_pred_w_final <- predict(elec_model, elec_rdts[1:500], elec_cov[1:500, , drop = FALSE],
      type = "probs", final_pred = TRUE
    )
    probs_pred_wo_final <- predict(elec_model, elec_rdts[1:500], elec_cov[1:500, , drop = FALSE],
      type = "probs", final_pred = FALSE
    )
    expect_equal(
      nrow(probs_pred_w_final),
      500 + 1
    )
    expect_equal(
      nrow(probs_pred_wo_final),
      500
    )
    expect_identical(
      probs_pred_wo_final,
      probs_pred_w_final[-length(pred_w_final), , drop = FALSE]
    )
  }
})
