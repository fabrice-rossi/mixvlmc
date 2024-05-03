test_that("dts_list conversion works", {
  withr::local_seed(0)
  pre_dl <- vector(length = 10, mode = "list")
  dl_states <- sample(letters, 10)
  for (k in 1:10) {
    pre_dl[[k]] <- sample(dl_states, 150 + sample(50:100, 1), replace = TRUE)
  }
  dl <- dts_list(pre_dl, vals = dl_states)
  expect_s3_class(dl, "dts_list")
  expect_length(dl, 10)
  expect_equal(states(dl), dl_states)
  for (k in seq_along(dl)) {
    a_dts <- dl[[k]]
    expect_s3_class(a_dts, "dts")
    expect_length(a_dts, length(pre_dl[[k]]))
    expect_equal(dts_data(a_dts), pre_dl[[k]])
    expect_equal(states(a_dts), dl_states)
  }
  expect_equal(lengths(dl), lengths(pre_dl))
  pre_dl_dts <- lapply(pre_dl, \(x) dts(x, vals = dl_states))
  dl2 <- dts_list(pre_dl_dts)
  expect_s3_class(dl2, "dts_list")
  expect_length(dl2, 10)
  expect_equal(states(dl2), dl_states)
  for (k in seq_along(dl2)) {
    a_dts <- dl2[[k]]
    expect_s3_class(a_dts, "dts")
    expect_length(a_dts, length(pre_dl[[k]]))
    expect_equal(dts_data(a_dts), pre_dl[[k]])
    expect_equal(states(a_dts), dl_states)
  }
})

test_that("errors are detected", {
  ## empty list
  expect_error(dts_list(list()))
  ## not a list
  expect_error(dts_list(1:4))
  ## inconsistent state space
  expect_error(dts_list(list(0:1, 3:4)))
  ## not a dts
  expect_error(dts_list(list(list())))
  ## unknown states
  expect_error(dts_list(
    list(
      sample(0:1, 10, replace = TRUE),
      sample(0:1, 10, replace = TRUE)
    ),
    vals = 2:3
  ))
})

test_that("dts_list printing works", {
  withr::local_seed(10)
  pre_dl <- vector(length = 15, mode = "list")
  dl_states <- sample(letters, 10)
  for (k in 1:15) {
    pre_dl[[k]] <- sample(dl_states, 150 + sample(50:100, 1), replace = TRUE)
  }
  dl <- dts_list(pre_dl, vals = dl_states)
  expect_snapshot(print(dl))
  expect_snapshot(print(dl[1:5]))
})
