plot.prediction <-
function (x, conf.level = 0.95, xlab = NULL, interval.type = "two-sided",
    ...)
{
    par(mar = c(5.1, 4.1, 4.1, 2.1) + c(0, 1, 0, 0))
    if (is.null(xlab))
        xlab <- paste("t", sep = "")
    ylab <- paste("f(t | DATA)", sep = "")
    predict.posterior.out <- predict.posterior(x,
        ...)
    distribution <- x$distribution
    if  (is.even(numdist(distribution))) {
        x <- exp(predict.posterior.out$x)
  } else {
        x <- predict.posterior.out$x
    }
    yrange <- range(predict.posterior.out$post.pred.den)
    plot(range(x), c(0, 1.1 * yrange[2]), type = "n", xlab = xlab,
        ylab = "", cex = 1.5, yaxt = "n", yaxs = "i")
    lines(x, predict.posterior.out$post.pred.den, lwd = 2)
    abline(h = 0)
    switch(interval.type, `two-sided` = {
        lowerq <- (1 - conf.level)/2
        lower.bound <- approx(predict.posterior.out$post.pred.cdf,
            x, lowerq)$y
        if (!is.na(lower.bound)) abline(v = lower.bound, lty = 2)
        upperq <- 1 - (1 - conf.level)/2
        upper.bound <- approx(predict.posterior.out$post.pred.cdf,
            x, upperq)$y
        if (!is.na(upper.bound)) abline(v = upper.bound, lty = 2)
        cat("The", percent.conf.level(conf.level), "prediction interval is:",
            format(c(lower.bound, upper.bound), digits = 4),
            "\n")
    }, lower = {
        lowerq <- (1 - conf.level)
        lower.bound <- approx(predict.posterior.out$post.pred.cdf,
            x, lowerq)$y
        if (!is.na(lower.bound)) abline(v = lower.bound, lty = 2)
        cat("The", percent.conf.level(conf.level), "lower prediction bound is:",
            format(c(lower.bound), digits = 4), "\n")
    }, upper = {
        upperq <- conf.level
        upper.bound <- approx(predict.posterior.out$post.pred.cdf,
            x, upperq)$y
        if (!is.na(upper.bound)) abline(v = upper.bound, lty = 2)
        cat("The", percent.conf.level(conf.level), "upper prediction bound is:",
            format(c(upper.bound), digits = 4), "\n")
    }, none = {
    }, stop("illegal interval type"))
}
