#' @export
summary.alt.plan.values <-
function (object, ...)
{
    cat("\n\nAccelerated test planning values \n")
    distribution <- object$distribution
    cat("\nDistribution: ", distribution, " \n")
    cat("Relationship: ", object$relationship, " \n")
    cat("Time units: ", object$time.units, " \n \n")
    cat("For a censoring time of", object$censor.time, object$time.units,
        " \n")
    cat("the failure probability at", paste(object$accelvar, object$accelvar.units),
        " is: ", format(object$probs), " \n")
    cat("\nIntercept is: ", format(object$beta0), " ", " \n")
    if (is.null(object$betavec))
        stop("\nOld style plan values object---need to remake\n")
    cat("Relationship coefficients = ", paste(format(object$betavec),
        collapse = ","), " \n \n")
    if (numdist(distribution) == 2) {
        cat("weibull.beta = ", format(object$beta), " \n")
    }
    cat("sigma = ", format(object$sigma), " \n")
    if (!is.null(object$use.conditions))
        cat("Use conditions:", paste(object$use.conditions, object$accelvar.units,
            collapse = ", "), "\n")
    invisible()
}
