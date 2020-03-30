library(mlr3learners.mboost)

test_that("classif.gamboost", {
  learner = lrn("classif.gamboost")
  fun = mboost::gamboost
  exclude = c(
    "formula", # .train
    "data", # .train
    "na.action", # Only na.omit and na.fail available
    "weights", # .train
    "control", # mboost::boost_control
    "..."
  )

  result = run_paramtest(learner, fun, exclude)
  expect_true(result, info = paste0("Missing parameters:\n",
    paste0(result$missing, collapse = "\n")))
})

test_that("classif.gamboost_boost_control", {
  learner = lrn("classif.gamboost")
  fun = mboost::boost_control
  exclude = c(
    "stopintern", # ?
    "center", # deprecated
    "trace", # ?
    "..."
  )

  result = run_paramtest(learner, fun, exclude)
  expect_true(result, info = paste0("Missing parameters:\n",
    paste0(result$missing, collapse = "\n")))
})
