#' Title
#'
#' @param fcn 
#' @param fcn.arg2 
#' @param gmle.out 
#' @param extrapolate 
#' @param ptwise.if.simult 
#' @param conf.level 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Fan.ld <- frame.to.ld(fan,
#'                       response.column = 1, 
#'                       censor.column = 2, 
#'                       case.weight.column = 3,
#'                       time.units = "Hours")
#' 
#' Fan.egeng.gmle.out <- FillRegion(Fan.egeng.gmle.out,
#'                                  nbound = 10,
#'                                  iter = 500)
#' 
#' summary(Fan.egeng.gmle.out.jcr)
#' 
#' names(Fan.egeng.gmle.out.jcr)
#' 
#' basic.gmleprobplot(Fan.ld,distribution = "egeng",
#'                    xlim = c(200,99999),
#'                    ylim = c(.0011,.69),
#'                    xxx.mle.out = Fan.egeng.gmle.out,
#'                    my.title = "",
#'                    cexlab = 1.5,
#'                    conlev = .95,
#'                    ciMethod = "lr.approx",
#'                    length.time.vec = 2)
#' 
#' Fan.weibull.gmle.out <- ls2.mle(Fan.ld, distribution = "weibull")
#' 
#' 
#' Fan.weibull.gmle.out <-  FillRegion(Fan.weibull.gmle.out,
#'                                     nbound = 4,
#'                                     iter = 50,
#'                                     cull = 2 )
#' 
#' fcn = function(theta,time,distribution){
#'   
#'       f.phibf((log(time)-theta[1]) / exp(theta[2]),
#'                       distribution = "Weibull")
#'   
#' }
#' 
#' Fr.conf(fcn, 
#'         fcn.arg2 = log.seq(200,2000,length=10),
#'         gmle.out = Fan.weibull.gmle.out,
#'         ptwise = T)
#' 
#' Fr.conf(fcn,
#'         fcn.arg2 = log.seq(200,2000,length = 10),
#'         gmle.out = Fan.weibull.gmle.out,  
#'         ptwise = T, 
#'         extrapolate = T)
#' 
#' }
Fr.conf <-
function (fcn, 
          fcn.arg2, 
          gmle.out, 
          extrapolate = F, 
          ptwise.if.simult = F,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, ...)
{
    jcrname <- gmle.out$jcrname
    
    if (is.null(gmle.out$jcrname) && !exists("jcrname")){
      
        stop("The joint likelihood ratio confidence region (jcrname) \nis not available")
      
    }
    jcr <- eval(parse(text = jcrname))
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
