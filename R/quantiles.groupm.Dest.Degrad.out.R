#' @export
quantiles.groupm.Dest.Degrad.out <-
function (x, FailLevel, use.condition, prob.vec = as.numeric(ClistToVec(GetSMRDDefault("SMRD.DefaultQuantileList"))),
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, digits = GetSMRDDefault("SMRD.DigitsPrinted"),
    to = 0.999, add.title = NULL, printem = T,...)
{
    func.call <- match.call()
    use.condition <- string.to.frame(use.condition)
    distribution <- x$model$distribution
    distribution.string <- paste("Dead-on-Arrival-", distribution,
        sep = "")
    results <- fx.ADDT.life.quantile(theta.hat = x$origparam,
        p = prob.vec, distribution = distribution, FailLevel = FailLevel,
        xuse = use.condition, transformation.response = x$model$transformation.response,
        transformation.x = x$model$transformation.groupm.Dest.Degrad.out,
        transformation.time = x$model$transformation.time)
    conf.char <- percent.conf.level(conf.level)
    conf.int.title <- paste("\nPointwise Approximate ", conf.char,
        " Confidence Intervals", sep = "")
    the.xmat <- xmat(x$data.ld)
    the.time.units <- get.time.units(x$data.ld)
    my.title <- paste(get.data.title(x$data.ld),
        add.title, "\n", "Parametric ML Failure-time Quantile Estimates \nat ",
        paste(signif(use.condition, 3), colnames(the.xmat)[-ncol(the.xmat)],
            collapse = ", "), "\nfor failure defined at a level of ",
        FailLevel, " ", get.response.units(x$data.ld),
        conf.int.title, "\n", sep = "")
    results <- f.gendeltamethod(vcv.theta = x$origparamvcv,
        theta = x$origparam, fx.ADDT.life.quantile,
        p = prob.vec, distribution = distribution, FailLevel = FailLevel,
        xuse = use.condition, transformation.response = x$model$transformation.response,
        transformation.groupm.Dest.Degrad.out = x$model$transformation.groupm.Dest.Degrad.out,
        transformation.time = x$model$transformation.time)
    ci.result <- compute.confidence.interval(param = results$vec,
        stderr = results$se, kodet = 2, conf.level = conf.level)
    if (is.null(x$dummy)) {
        the.table <- cbind(p = prob.vec, Quanhat = results$vec,
            Stderror = results$se, Lower = ci.result$fun.lower,
            Upper = ci.result$fun.upper)
        colnames(the.table) <- c("p", the.time.units, "Std.Err.", paste(conf.char, "Lower"),
                            paste(conf.char, "Upper"))
    }
    else {
        the.table <- cbind(p = prob.vec, Quanhat = results$vec)
        colnames(the.table) <- c("p", "Quanhat")
    }

    attr(the.table, "title") <- my.title
    attr(the.table, "distribution") <- distribution.string
    attr(the.table, "groupm.Dest.Degrad.out") <- x
    oldClass(the.table) <- c("quantiles.out", "estimates.out", "matrix")
    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}
