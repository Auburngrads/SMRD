Fr.conf <-
function (fcn, fcn.arg2, gmle.out, extrapolate = F, ptwise.if.simult = F,
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, ...)
{
    jcrname <- gmle.out$jcrname
    if (is.null(gmle.out$jcrname) && !exists(jcrname))
        stop("The joint likelihood ratio confidence region (jcrname) \n                       is not available")
    jcr <- get(envir = .frame0, jcrname)
    param <- jcr$conf.reg
    type <- jcr$type
    l.bounds <- jcr$l.bounds
    dim <- length(l.bounds)
    jcr.conf.level <- jcr$conf.level
    max.log.like <- gmle.out$max.log.like
    if (conf.level > jcr.conf.level)
        stop("The desired confidence level of the confidence intervals\n                  must not be greater than the confidence level of the \n                  generated points")
    else if (conf.level < jcr.conf.level) {
        if (type == "pointwise") {
            cut.off <- 0.5 * qchisq(conf.level, 1) - max.log.like
            param <- param[param[, dim + 1] < cut.off, byrow = T]
      } else {
            if (ptwise.if.simult) {
                cut.off <- 0.5 * qchisq(conf.level, 1) - max.log.like
                param <- param[param[, dim + 2] < cut.off & param[,
                  dim + 1] == 1, byrow = T]
          } else {
                cut.off <- 0.5 * qchisq(conf.level, dim) - max.log.like
                param <- param[param[, dim + 2] < cut.off, byrow = T]
            }
        }
  } else {
        if (type == "simultaneous" && ptwise.if.simult) {
            param <- param[param[, dim + 1] == 1, byrow = T]
        }
    }
    len.fcn.arg2 <- length(fcn.arg2)
    ci <- matrix(0, nrow = 2, ncol = len.fcn.arg2)
    if (extrapolate)
        ci.extrap <- matrix(0, nrow = 2, ncol = len.fcn.arg2)
    for (i in 1:len.fcn.arg2) {
        y <- apply(param, 1, fcn, fcn.arg2[i], gmle.out, ...)
        ci[, i] <- range(y)
        if (extrapolate)
            ci.extrap[, i] <- QQbeta.fr(y, dim)
        if (i%%ceiling(0.1 * len.fcn.arg2) == 0 || i == len.fcn.arg2) {
            cat(round((100 * i)/len.fcn.arg2), "% completed \n")
        }
    }
    if (extrapolate) {
        lower <- ci.extrap[1, ]
        upper <- ci.extrap[2, ]
        lower.maxmin <- ci[1, ]
        upper.maxmin <- ci[2, ]
  } else {
        lower <- ci[1, ]
        upper <- ci[2, ]
        lower.maxmin <- NULL
        upper.maxmin <- NULL
    }
    return(list(lower = lower, upper = upper, lower.maxmin = lower.maxmin,
        upper.maxmin = upper.maxmin))
}
