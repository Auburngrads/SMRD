truncadj <-
function (cdpoints.out, mlest.out,debug1= T)
{
    if (mlest.out$ierfit + mlest.out$iervcv > 0) {
        warning("Problems in parametric ML estimation--no truncation adjustment being done")
        return(cdpoints.out)
    }
    right.trun.cond <- cdpoints.out$right.trun.cond
    left.trun.cond <- cdpoints.out$left.trun.cond
    log.of.data <- is.even(numdist(mlest.out$distribution))
    if (debug1) {
        xcdpoints.out <- list(yplot = cdpoints.out$yplot, 
                              pplot = cdpoints.out$pplot)
        print(xcdpoints.out)
    }
    if (!is.null(left.trun.cond)) {
        plot.quantiles.out <- plot.quantiles(mlest.out, 
                                                     conf.level = 0,
                                                     xrange = left.trun.cond, 
                                                     plotem = F, 
                                                     timelen = 1,
                                                     log.of.data = log.of.data)
        
        ftlower <- plot.quantiles.out$dist.probs
        se.ftlower <- plot.quantiles.out$stderror
  } else {
        ftlower <- 0
        se.ftlower <- 0
    }
    if (!is.null(right.trun.cond)) {
        plot.quantiles.out <- plot.quantiles(mlest.out, conf.level = 0,
            xrange = right.trun.cond, plotem = F, timelen = 1,
            log.of.data = log.of.data, allow.extremes = T)
        ftupper <- plot.quantiles.out$dist.probs
        se.ftupper <- plot.quantiles.out$stderror
  } else {
        ftupper <- 1
        se.ftupper <- 0
    }
    cdpoints.out$sdplot <- sqrt((ftupper - ftlower)^2 * cdpoints.out$sdplot +
        (1 - ftlower)^2 * se.ftlower^2 + ftupper^2 * se.ftupper^2)
    cdpoints.out$pplot <- (ftlower + (ftupper - ftlower) * cdpoints.out$pplot)
    correct.vec <- c(ftlower = ftlower, left.trun.cond = left.trun.cond,
        ftupper = ftupper, right.trun.cond = right.trun.cond)
    if (debug1) {
        print(correct.vec)
        xcdpoints.out <- list(distprobs = plot.quantiles.out$dist.probs,
            yplot = cdpoints.out$yplot, corrected.pplot = cdpoints.out$pplot,
            left.trun.cond = cdpoints.out$left.trun.cond, right.trun.cond = cdpoints.out$right.trun.cond,
            ftlower = ftlower, ftupper = ftupper)
        print(xcdpoints.out)
    }
    attr(cdpoints.out, "trunc.correct.string") <- "\nwith truncation-corrected nonparametric estimate"
    return(cdpoints.out)
}
