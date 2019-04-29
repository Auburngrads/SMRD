theta.start.est <-
function (data, distribution)
{
    ndist <- numdist(distribution)
    y <- Response(data)
    if (generic.distribution(distribution) == "exponential") {
        the.case.weights <- case.weights(data)
        theta.start <- c(logb(sum(y * the.case.weights)/sum(the.case.weights)),
            1)
        return(theta.start)
    }
    cdfest.out <- cdfest(data)
    if (length(cdfest.out$q) <= 10) {
        if  (is.even(ndist))
            return(c(mean(logb(as.matrix(y)[, 1])), sqrt(var(logb(as.matrix(y)[,
                1])))))
        else return(c(mean(as.matrix(y)[, 1]), sqrt(var(as.matrix(y)[,
            1]))))
    }
    cdpoints.out <- cdpoints(cdfest(data))
    if  (is.even(ndist))
        trans.resp <- logb(cdpoints.out$yplot)
    else trans.resp <- cdpoints.out$yplot
    the.quantiles <- quant(cdpoints.out$pplot, distribution)
    theta.start.est <- coefficients(lm(trans.resp ~ the.quantiles))
    return(theta.start.est)
}
