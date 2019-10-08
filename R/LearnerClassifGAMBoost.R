#' @title Boosted Generalized Additive Classification Learner
#'
#' @aliases mlr_learners_classif.gamboost
#' @format [R6::R6Class] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' A [LearnerClassif] for a classification gamboost implemented in [mboost::gamboost()] in package \CRANpkg{mboost}.
#'
#' @references
#' Peter Buhlmann and Bin Yu (2003)
#' Boosting with the L2 Loss: Regression and Classification
#' Journal of the American Statistical Association
#' \url{https://doi.org/10.1198/016214503000125}
#'
#' @export
LearnerClassifGAMBoost = R6Class("LearnerClassifGAMBoost", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new(id = "baselearner", default = "bbs", levels = c("bbs", "bols", "btree"), tags = "train"),
          ParamInt$new(id = "dfbase", default = 4L, tags = "train"),
          ParamDbl$new(id = "offset", default = NULL, special_vals = list(NULL), tags = "train"),
          ParamFct$new(id = "family", default = c("Binomial"), levels = c("Binomial", "AdaExp", "AUC"), tags = "train"),
          ParamFct$new(id = "link", default = "logit", levels = c("logit", "probit"), tags = "train"), # Only for family = Binomial
          ParamFct$new(id = "type", default = "adaboost", levels = c("glm", "adaboost"), tags = "train"), # Only for family = Binomial
          ParamInt$new(id = "mstop", default = 100, tags = "train"),
          ParamDbl$new(id = "nu", default = 0.1, tags = "train"),
          ParamFct$new(id = "risk", default = "inbag", levels = c("inbag", "oobag", "none"), tags = "train")
        )
      )

      super$initialize(
        id = "classif.gamboost",
        packages = "mboost",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass")
      )
    },

    train_internal = function(task) {

      # Default family in mboost::gamboost is not useable for twoclass
      if (is.null(self$param_set$values$family)) {
        self$param_set$values$family = "Binomial"
      }

      pars = self$param_set$get_values(tags = "train")
      pars_boost = pars[which(names(pars) %in% formalArgs(mboost::boost_control))]
      pars_gamboost = pars[which(names(pars) %in% formalArgs(mboost::gamboost))]
      pars_binomial = pars[which(names(pars) %in% formalArgs(mboost::Binomial))]

      f = task$formula()
      data = task$data()

      if ("weights" %in% task$properties) {
        pars_gamboost = insert_named(pars_gamboost, list(weights = task$weights$weight))
      }

      pars_gamboost$family = switch(pars_gamboost$family,
        Binomial = invoke(mboost::Binomial, .args = pars_binomial),
        AdaExp = mboost::AdaExp(),
        AUC = mboost::AUC())

      # Predicted probabilities refer to the last factor level
      if (self$predict_type == "prob") {
        levs = c(task$negative, task$positive)
        data[[task$target_names]] = factor(data[[task$target_names]], levs)
      }

      ctrl = invoke(mboost::boost_control, .args = pars_boost)

      withr::with_package("mboost", { # baselearner argument requires attached mboost package
        invoke(mboost::gamboost, formula = f, data = data, control = ctrl, .args = pars_gamboost)
      })
    },

    predict_internal = function(task) {
      family = self$param_set$values$family
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "prob" && (family == "AdaExp" || family == "AUC")) {
        stopf("The selected family %s does not support probabilities", family)
      }

      if (self$predict_type == "response") {
        p = invoke(predict, self$model, newdata = newdata, type = "class")
        PredictionClassif$new(task = task, response = p)
      } else {
        p = invoke(predict, self$model, newdata = newdata, type = "response")
        p = matrix(c(p, 1 - p), ncol = 2L, nrow = length(p))
        colnames(p) = task$class_names
        PredictionClassif$new(task = task, prob = p)
      }
    }
  )
)
