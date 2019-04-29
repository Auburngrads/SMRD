PrSDzero.failure.plan <-
function (quantile = 0.1, TrueRelvec = seq(0.9, 0.9999, by = 0.001), 
    conlevvec = c(0.5, 0.8, 0.9, 0.95), xrange = NULL, yrange = NULL, 
    ltyvec = c(1, 1, 1, 1, 1, 1, 1, 1)) 
{
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    pmat <- matrix(NA, nrow = length(TrueRelvec), ncol = length(conlevvec))
    for (i in 1:length(conlevvec)) {
        pmat[, i] <- TrueRelvec^(log(1 - conlevvec[i])/log(1 - 
            quantile))
    }
    if (is.null(xrange)) 
        xrange <- range(TrueRelvec)
    if (is.null(yrange)) 
        yrange <- range(pmat)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "", cex = 1.5)
    title(xlab = "Actual Reliability", cex = 1.5)
    title(ylab = "Probability of Successful Demonstration", cex = 1.5)
    holdout <- c(1:10)
    for (i in 1:length(conlevvec)) {
        lines(TrueRelvec[-holdout], pmat[-holdout, i], lty = ltyvec[i], 
            lwd = 2)
        text(x.loc(0.1), pmat[9, i], paste(as.character(floor(100 * 
            conlevvec[i])), "%", sep = ""))
    }
    title(main = paste("Successfully Demonstrating That R=", 
        1 - quantile))
}
