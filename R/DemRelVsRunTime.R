DemRelVsRunTime <-
function (beta, n, conlevvec = c(0.5, 0.8, 0.9, 0.95), xrange = NULL, 
    yrange = NULL, ltyvec = c(1, 1, 1, 1, 1, 1, 1, 1)) 
{
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    kvec <- seq(0.5, 4, length = 200)
    demoTime = 4000
    pmat <- matrix(NA, nrow = length(kvec), ncol = length(conlevvec))
    TestTime <- demoTime * kvec
    for (i in 1:length(conlevvec)) {
        pmat[, i] <- exp(log(1 - conlevvec[i])/((kvec^beta) * 
            n))
    }
    if (is.null(xrange)) 
        xrange <- range(TestTime)
    if (is.null(yrange)) 
        yrange <- range(pmat)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "", cex = 1.3)
    title(xlab = "Failure-Free Testing Cycles", cex = 1.5)
    title(ylab = "Demonstrated Reliability at 4000 Cycles", cex = 1.5)
    holdout <- c(1:10)
    for (i in 1:length(conlevvec)) {
        lines(TestTime[-holdout], pmat[-holdout, i], lty = ltyvec[i], 
            lwd = 2)
        text(x.loc(0.06), pmat[10, i], paste(as.character(floor(100 * 
            conlevvec[i])), "%", sep = ""))
    }
    title(main = paste("Testing n = ", n, "Units Assuming beta = ", 
        beta))
}
