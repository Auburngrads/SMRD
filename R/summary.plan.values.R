summary.plan.values <-
function (object,...)
{
    distribution <- object$distribution
    cat("\nDistribution: ", distribution, " \n")
    cat("Time units are: ", object$time.units, " \n \n")
    for (i in 1:length(object$probs)) {
        cat("The ", object$probs[i], "quantile is: ", format(object$times[i]),
            " \n")
    }
    if (generic.distribution(distribution) == "weibull") {
        cat("\nShape parameter beta is: ", format(object$beta),
            " \n")
        cat("Characteristic life eta is: ", format(object$eta),
            " ", object$time.units, " \n \n")
    }
    cat("mu = ", format(object$mu), " \n")
    cat("sigma = ", format(object$sigma), " \n")
    invisible()
}
