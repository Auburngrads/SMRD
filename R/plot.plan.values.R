#' Plot reliability life test
#'
#' @param x 
#' @param censor.time 
#' @param my.title 
#' @param grids 
#' @param quantile.mark 
#' @param number.points 
#' @param perc.low 
#' @param perc.high 
#' @param cex 
#' @param xlim 
#' @param ylim 
#' @param title.option 
#' @param xlab 
#' @param digits 
#' @param ... 
#'
#' @return NULL
#' @export
plot.plan.values <-
function (x, censor.time = NULL, my.title, grids = 2,
    quantile.mark = NULL, number.points = 20, perc.low = 0.01,
    perc.high = 0.99, cex = 1, xlim = NULL, ylim = NULL,
    title.option = GetSMRDDefault("SMRD.TitleOption"), xlab = x$time.units, digits = GetSMRDDefault("SMRD.DigitsPrinted"),...)
{
    mu <- x$mu
    sigma <- x$sigma
    distribution <- x$distribution
    if (is.null(xlab))
        xlab <- "Time"
    if (missing(my.title)) {
        if (numdist(distribution) == 2) {
            my.title <- paste(distribution, "Distribution with eta=",
                format(exp(mu), digits = digits), " and beta=",
                format(1/sigma, digits = digits))
      } else {
            my.title <- paste(distribution, "Distribution with mu=",
                format(mu, digits = digits), " and sigma=", format(sigma,
                  digits = digits))
        }
    }
    logtime.lower <- mu + sigma * quant(perc.low, distribution)
    logtime.upper <- mu + sigma * quant(perc.high, distribution)
    if (is.null(xlim))
        xlim <- c(logtime.lower, logtime.upper)
    logtime <- seq(logtime.lower, logtime.upper, length = number.points)
    if (is.logdist(distribution)) {
        xlim <- exp(c(logtime.lower, logtime.upper))
        realtime <- exp(logtime)
        if (!is.null(censor.time))
            censor.time <- logb(censor.time)
  } else {
        if (is.null(xlim))
            xlim <- c(logtime.lower, logtime.upper)
        realtime <- logtime
    }
    ylim <- c(perc.low, perc.high)
    rel.test.plot.setup <- function(logtime, mu, sigma, censor.time,
        quantile.mark, grids, perc.high, distribution, xlim,
        ylim, my.title = my.title, cex, xlab = xlab, title.option = title.option,
        ...) {
        log.of.data <- probplot.setup(distribution = distribution,
            xlim = xlim, ylim = ylim, my.title = my.title,
            grids = grids, sub.title = NULL, cex = cex, linear.axes = F,
            xlab = xlab, title.option = title.option, slope.axis = F,
            ...)
        dist.cdf <- wqmf.phibf((logtime - mu)/sigma, distribution)
        old.options <- options(digits = 3)
        pop.cdf1 <- wqmf.phibf((logtime - mu)/sigma, distribution)
        lines(logtime, quant(pop.cdf1, distribution), type = "l",
            lwd = 5, col = 6)
        if (!is.null(censor.time)) {
            abline(v = censor.time, col = 1, lwd = 2, lty = 1)
          axis(side = 3, at = censor.time, labels = FALSE, adj = 1.0, col = 1, lwd = 2, tck = -.06)  
          mtext(side = 3, expression(Censor~Time%->%''), at = censor.time, adj = 1, line = .15)  
        }
        if (!is.null(quantile.mark)) {
            abline(h = quant(quantile.mark, distribution), col = 3,
                lwd = 3, lty = 4)
        }
    }
    log.of.data <- rel.test.plot.setup(logtime, mu, sigma, censor.time,
        quantile.mark, grids, perc.high, distribution, xlim,
        ylim, my.title = my.title, cex, xlab = xlab, title.option = title.option)
    CheckPrintDataName()
    invisible()
}
