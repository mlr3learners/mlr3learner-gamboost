#' @title Boosted Generalized Additive Classification Learner
#'
#' @name mlr_learners_classif.gamboost
#'
#' @description
#' Boosted generalized additive classification learner.
#' Calls [mboost::gamboost()] from package \CRANpkg{mboost}.
#'
#' @templateVar id classif.gamboost
#' @template section_dictionary_learner
#'
#' @references
#' \cite{mlr3learners.mboost}{buhlmann_2003}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifGAMBoost = R6Class("LearnerClassifGAMBoost",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Create a `LearnerClassifGAMBoost` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new(id = "baselearner", default = "bbs",
            levels = c("bbs", "bols", "btree"), tags = "train"),
          ParamInt$new(id = "dfbase", default = 4L, tags = "train"),
          ParamDbl$new(id = "offset", default = NULL,
            special_vals = list(NULL), tags = "train"),
          ParamFct$new(id = "family", default = c("Binomial"),
            levels = c("Binomial", "AdaExp", "AUC"), tags = "train"),
          ParamFct$new(id = "link", default = "logit",
            levels = c("logit", "probit"), tags = "train"),
          ParamFct$new(id = "type", default = "adaboost",
            levels = c("glm", "adaboost"), tags = "train"),
          ParamInt$new(id = "mstop", default = 100, tags = "train"),
          ParamDbl$new(id = "nu", default = 0.1, tags = "train"),
          ParamFct$new(id = "risk", default = "inbag",
            levels = c("inbag", "oobag", "none"), tags = "train"),
          ParamUty$new(id = "oobweights", default = NULL, tags = "train")
        )
      )
      ps$add_dep("type", "family", CondEqual$new("Binomial"))
      ps$add_dep("link", "family", CondEqual$new("Binomial"))
      ps$add_dep("oobweights", "risk", CondEqual$new("oobag"))

      super$initialize(
        id = "classif.gamboost",
        packages = "mboost",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass"),
        man = "mlr3learners.mboost::mlr_learners_classif.gamboost"
      )
    }
  ),

  private = list(

    .train = function(task) {

      # Default family in mboost::gamboost is not useable for twoclass
      if (is.null(self$param_set$values$family)) {
        self$param_set$values$family = "Binomial"
      }

      pars = self$param_set$get_values(tags = "train")
      pars_boost = pars[which(names(pars) %in%
        formalArgs(mboost::boost_control))]
      pars_gamboost = pars[which(names(pars) %in%
        formalArgs(mboost::gamboost))]
      pars_binomial = pars[which(names(pars) %in%
        formalArgs(mboost::Binomial))]

      f = task$formula()
      data = task$data()

      if ("weights" %in% task$properties) {
        pars_gamboost = insert_named(pars_gamboost,
          list(weights = task$weights$weight))
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

      # baselearner argument requires attached mboost package
      withr::with_package("mboost", {
        invoke(mboost::gamboost, formula = f, data = data, control = ctrl,
          .args = pars_gamboost)
      })
    },

    .predict = function(task) {
      family = self$param_set$values$family
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "prob" && (family == "AdaExp" ||
        family == "AUC")) {
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
