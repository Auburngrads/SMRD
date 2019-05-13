#' @export
summary.ADDT.plan.values <-
function (object,...)
{
    cat("\n\nDestructive degradation test planning values \n")
    cat("\nDistribution: ", object$distribution, " \n\n")
    cat("\nResponse units: ", object$response.units, " \n")
    cat("Transformation on the response: ", object$transformation.response,
        " \n \n")
    cat("Time units: ", object$time.units, " \n")
    cat("Transformation on time: ", object$transformation.time,
        " \n \n")
    cat("Accelerating variable units: ", paste(object$accelvar.units,
        collapse = ", "), "\n")
    cat("Relationship(s): ", paste(object$transformation.x, collapse = ", "),
        " \n \n")
    cat("Parameters:\n")
    cat("ADDT Intercept beta0 = ", format(object$theta.vec["beta0"]),
        " \n")
    cat("ADDT slope beta1 = ", format(object$theta.vec["beta1"]),
        " \n")
    beta2.names <- paste("beta", seq(2, length(object$accelvar.units) +
        1), sep = "")
    beta2.vec <- object$theta.vec[beta2.names]
    if (length(beta2.vec) > 1)
        beta2.vec.name <- "coefficients beta2vec"
    else beta2.vec.name <- "coefficient beta2"
    cat("ADDT slope relationship", beta2.vec.name, "=", paste(format(beta2.vec),
        collapse = ", "), " \n")
    cat("ADDT sigma = ", format(object$sigma), " \n")
    if (!is.null(object$FailLevel))
        cat("ADDT Failure definition level = ", format(object$FailLevel),
            " \n")
    if (!is.null(object$use.condition)) {
        cat("ADDT Use condition:", paste(paste(format(object$use.condition),
            object$accelvar.units), collapse = ", "), " \n")
        transformation.x <- object$transformation.x
        mod.trans.x <- fix.inverse.relationship(transformation.x)
        beta2.x <- 0
        for (i in 1:length(transformation.x)) {
            beta2.x <- beta2.x + beta2.vec[i] * f.relationship(object$use.condition[i],
                subscript.relationship(mod.trans.x, i))
        }

        cat("Slope at ADDT use condition", paste(format(object$use.condition),
            object$accelvar.units, collapse = ", "), "is",
            format(object$theta.vec["beta1"] * exp(as.numeric(beta2.x))),
            "\n")
    }
    if (is.null(object$theta.vec.cr) & is.null(object$beta.cr) &
        is.null(object$sigma.cr))
        return()
    cat("\nCompeting risk parameters for the censoring function\n")
    beta2.names.cr <- paste(beta2.names, ".cr", sep = "")
    beta2.vec.cr <- object$theta.vec.cr[beta2.names.cr]
    if (length(beta2.vec.cr) > 1)
        beta2.vec.name.cr <- "coefficients beta2vec.cr"
    else beta2.vec.name.cr <- "coefficient beta2.cr"
    cat("ADDT slope.cr relationship", beta2.vec.name.cr, "=",
        paste(format(beta2.vec.cr), collapse = ", "), " \n")
    cat("ADDT sigma.cr = ", format(object$sigma.cr), " \n")
    beta2.x.cr <- 0
    for (i in 1:length(transformation.x)) {
        beta2.x.cr <- beta2.x.cr + beta2.vec.cr[i] * f.relationship(object$use.condition[i],
            subscript.relationship(mod.trans.x, i))
    }
    the.slope.cr <- object$theta.vec.cr["beta1.cr"] * exp(beta2.x.cr)
    cat("Slope.cr at ADDT use condition", paste(format(object$use.condition),
        object$accelvar.units, collapse = ", "), "is", format(the.slope.cr),
        "\n")
    invisible()
}
