#' @export
failure.probabilities.groupm.Dest.Degrad.out<-
function (x, FailLevel, use.condition, time.vec = NULL,
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, digits = GetSMRDDefault("SMRD.DigitsPrinted"),
    to = 0.999, add.title = NULL, printem = T,...)
{
    distribution <- x$model$distribution
    distribution.string <- paste("Dead-on-Arrival-", 
                                 distribution,
                                 sep = "")
    
    if (is.null(time.vec) || any(time.vec == Inf)) {
        time.vec <- get.time.vector(x, 
                                    distribution = distribution,
                                    number = 10)
    }
    func.call <- match.call()
    use.condition <- string.to.frame(use.condition)
    results <- fx.ADDT.life.failure.probability(theta.hat = x$origparam,
                                                time.vec = time.vec, 
                                                distribution = distribution, 
                                                FailLevel = FailLevel,
                                                xuse = use.condition, 
                                                transformation.response = x$model$transformation.response,
                                                transformation.x = x$model$transformation.x,
                                                transformation.time = x$model$transformation.time)
    
    conf.char <- percent.conf.level(conf.level)
    conf.int.title <- paste("\nPointwise Approximate ", conf.char,
                            " Confidence Intervals", sep = "")
    
    the.xmat <- xmat(x$data.ld)
    the.time.units <- get.time.units(x$data.ld)
    my.title <- paste(get.data.title(x$data.ld),
        add.title, "\n", "Parametric ML Distribution Failure Probability Estimates \nat ",
        paste(signif(use.condition, 4), colnames(the.xmat)[-ncol(the.xmat)],
            collapse = ", "), "\nfor failure defined at a level of ",
        FailLevel, " ", get.response.units(x$data.ld),
        conf.int.title, "\n", sep = "")
    results <- f.gendeltamethod(vcv.theta = x$origparamvcv,
        theta = x$origparam, fx.ADDT.life.failure.probability,
        time.vec = time.vec, distribution = distribution, FailLevel = FailLevel,
        xuse = use.condition, transformation.response = x$model$transformation.response,
        transformation.x = x$model$transformation.x,
        transformation.time = x$model$transformation.time)
    ci.result <- compute.confidence.interval(param = results$vec,
        stderr = results$se, kodet = 3, conf.level = conf.level)
    the.table <- cbind(Time = time.vec, Fhat = results$vec, Stderror = results$se,
        Lower = ci.result$fun.lower, Upper = ci.result$fun.upper)

    colnames(the.table) <- c(the.time.units,"Fhat", "Std.Err.", paste(conf.char, "Lower"),
                             paste(conf.char,"Upper"))
    attr(the.table, "title") <- my.title
    attr(the.table, "distribution") <- distribution.string
    attr(the.table, "x") <- x
    oldClass(the.table) <- c("failure.probabilities.out", "estimates.out",
        "matrix")
    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}
