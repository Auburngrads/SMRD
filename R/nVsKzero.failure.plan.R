nVsKzero.failure.plan <-
function (betavec = c(1, 1.5, 2, 3), quantile = 0.1, conlev = 0.99, 
    krange = c(1.5, 4), xrange = NULL, yrange = NULL, ltyvec = c(1, 
        3, 4, 5, 6)) 
{
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    kvec <- seq(krange[1], krange[2], length = 25)
    nmat <- matrix(NA, nrow = length(kvec), ncol = length(betavec))
    for (i in 1:length(kvec)) {
        nmat[i, ] <- (logb(1 - conlev)/logb(1 - quantile))/exp(logb(kvec[i]) * 
            betavec)
    }
    if (is.null(xrange)) 
        xrange <- range(kvec)
    if (is.null(yrange)) 
        yrange <- range(nmat)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "", cex = 1.5)
    title(xlab = "Test Length Factor k", cex = 1.5)
    title(ylab = "Number of Units Tested with Zero Failures", 
        cex = 1.5)
    for (i in 1:length(betavec)) {
        lines(kvec, nmat[, i], lty = ltyvec[i], lwd = 2)
        text(x.loc(0.95), nmat[nrow(nmat), i], as.character(betavec[i]))
    }
    title(main = paste("Sample Size Needed to Demonstrate R =", 
        1 - quantile, "\nas a Function of the Test Time-Length Factor"))
}
