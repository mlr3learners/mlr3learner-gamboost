run_paramtest = function(learner, fun, exclude = "...") {
  par_learner = c(learner$param_set$ids(), exclude)
  par_package = formalArgs(fun)

  missing = par_package[!par_package %in% par_learner]

  if(length(missing) > 0) {
    run = list(ok = FALSE, missing = missing)
    return(run)
  }
  return(TRUE)
}
