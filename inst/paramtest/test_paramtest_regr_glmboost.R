library(mlr3learners.mboost)

test_that("regr.glmboost", {
  learner = lrn("regr.glmboost")
  fun = mboost:::glmboost.formula
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "weights", # handled via mlr3
    "control", # handed to mboost::boost_control
    "..." # not used
  )

  result = run_paramtest(learner, fun, exclude)
  expect_true(result, info = paste0("Missing parameters:\n",
    paste0(result$missing, collapse = "\n")))
})

test_that("regr.glmboost_boost_control", {
  learner = lrn("regr.glmboost")
  fun = mboost::boost_control
  exclude = c(
    "center" # deprecated
  )

  result = run_paramtest(learner, fun, exclude)
  expect_true(result, info = paste0("Missing parameters:\n",
    paste0(result$missing, collapse = "\n")))
})
