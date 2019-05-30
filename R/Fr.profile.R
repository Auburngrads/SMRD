#' Title
#'
#' @param fcn 
#' @param gmle.out 
#' @param nlevels 
#' @param lowest.conf.level 
#' @param xlab 
#' @param distribution 
#' @param subtitle 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' BearingCage.ld <- frame.to.ld(bearingcage,
#'                               response.column = 1, 
#'                               censor.column = 2, 
#'                               case.weight.column = 3,
#'                               time.units = "Hours")
#' 
#' summary(BearingCage.ld)
#' 
#' BearingCage.weibull.gmle.out <- ls.mle(BearingCage.ld,
#'                                        distribution = "Weibull")
#' 
#' names(BearingCage.weibull.gmle.out)
#' 
#' BearingCage.weibull.gmle.out.jcr <- FillRegion(BearingCage.weibull.gmle.out,
#'                                                nbound = 4,
#'                                                iter = 500,
#'                                                cull = 2)
#' 
#' summary(BearingCage.weibull.gmle.out.jcr)
#' 
#' mleprobplot(BearingCage.ld,
#'             distribution = "Weibull",
#'             xlim = c(200,10000),
#'             ylim = c(.00031,.19),
#'             time.vec = log.seq(200,10000,20))
#' 
#' basic.gmleprobplot(BearingCage.ld,
#'                    distribution = "Weibull",
#'                    xxx.mle.out = BearingCage.weibull.gmle.out,
#'                    my.title = "",
#'                    cexlab = 1.5,
#'                    conf.level = 0.95,
#'                    length.time.vec = 20,
#'                    xlim = c(200,10000),
#'                    ylim = c(.00031,.19), 
#'                    time.vec = log.seq(200,10000,20),
#'                    ciMethod = "lr.approx")
#' 
#' basic.gmleprobplot(BearingCage.ld,
#'                    distribution = "Weibull",
#'                    xxx.mle.out = BearingCage.weibull.gmle.out,
#'                    my.title = "",
#'                    cexlab = 1.5,
#'                    conf.level = 0.95,
#'                    length.time.vec = 20, 
#'                    xlim = c(200,10000),
#'                    ylim = c(.00031,.19), 
#'                    time.vec = log.seq(200,10000,20))
#' 
#' profile.plot(Fr.profile(fcn <- function(x){x[1]},
#'                         BearingCage.weibull.gmle.out))
#' 
#' tmppro <- Fr.profile(fcn <- function(x){x[1]}, 
#'                      BearingCage.weibull.gmle.out)
#' 
#' 
#' }
Fr.profile <-
function (fcn, gmle.out, nlevels = 10, lowest.conf.level = 0.1,
    xlab = as.character(substitute(fcn)), distribution = gmle.out$model$distribution,
    subtitle = " ", ...)
{
    jcrname <- gmle.out$jcrname
    
    if (is.null(gmle.out$jcrname) && !exists("jcrname")){
        
        stop("The joint likelihood ratio confidence region (jcrname) is not available")
        
    }
    jcr <- eval(parse(text = jcrname))
    param <- jcr$conf.reg
    type <- jcr$type
    conf.level <- jcr$conf.level
    dim <- length(jcr$l.bounds)
    if (type == "pointwise")
        df <- 1
    else df <- dim
    npts <- nrow(param)
    max.log.like <- gmle.out$max.log.like
    col.log.like <- ncol(param)
    temp.seq <- seq(from = conf.level, to = lowest.conf.level,
        len = nlevels)
    chisq.seq <- qchisq(temp.seq, df)
    UL.limit <- matrix(0, nrow = nlevels, ncol = 2)
    for (i in 1:nlevels) {
        min.fcn <- NULL
        for (j in 1:npts) {
            if (param[j, col.log.like] < 0.5 * chisq.seq[i] -
                max.log.like) {
                theta <- param[j, 1:dim]
                new <- fcn(theta, ...)
                if (is.null(min.fcn)) {
                  min.fcn <- new
                  max.fcn <- new
                }
                if (new < min.fcn)
                  min.fcn <- new
                if (new > max.fcn)
                  max.fcn <- new
            }
        }
        UL.limit[i, ] <- c(min.fcn, max.fcn)
        if (i%%ceiling(0.1 * nlevels) == 0 || i == nlevels) {
            cat(round((100 * i)/nlevels), "% completed \n")
        }
    }
    x <- c(UL.limit[, 1], rev(UL.limit[, 2]))
    z <- exp(-0.5 * qchisq(temp.seq, 1))
    y <- c(z, rev(z))
    out <- list(x = x, y = y, xlab = xlab, distribution = distribution,
        number.parameters = dim, subtitle = subtitle)
    return(out)
}
