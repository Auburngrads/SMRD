DegCorr <-
function (degmat, time.column, unit.column, x.columns) 
{
    mean.matrix <- matrix(0, ncol = length(x.columns), nrow = length(unique(degmat[[time.column]])))
    cv.matrix <- matrix(0, ncol = length(x.columns), nrow = length(unique(degmat[[time.column]])))
    var.matrix <- matrix(0, ncol = length(x.columns), nrow = length(unique(degmat[[time.column]])))
    sample.size.matrix <- matrix(0, ncol = length(x.columns), 
        nrow = length(unique(degmat[[time.column]])))
    the.units <- unique(degmat[[unit.column]])
    number.units <- length(the.units)
    the.time.vec <- degmat[, time.column]
    unique.times <- unique(the.time.vec)
    for (tindex in 1:length(unique.times)) {
        for (j in 1:length(x.columns)) {
            the.j.sub.yvec <- degmat[degmat[[time.column]] == 
                unique.times[tindex], x.columns[j]]
            if (length(the.j.sub.yvec) > 0) 
                mean.matrix[tindex, j] <- mean(the.j.sub.yvec)
            var.matrix[tindex, j] <- var(the.j.sub.yvec)
            sample.size.matrix[tindex, j] <- length(the.j.sub.yvec)
        }
    }
    cv.matrix <- sqrt(var.matrix)/mean.matrix
    dimnames(mean.matrix) <- list(unique.times, x.columns)
    dimnames(var.matrix) <- dimnames(mean.matrix)
    dimnames(sample.size.matrix) <- dimnames(mean.matrix)
    dimnames(cv.matrix) <- dimnames(mean.matrix)
    cat("\n\nMean matrix\n")
    print(mean.matrix)
    cat("\n\nVariance matrix\n")
    print(var.matrix)
    cat("\n\nSample size matrix\n")
    print(sample.size.matrix)
    cat("\n\nCoefficient of variation matrix\n")
    print(cv.matrix)
    plot.matrix <- matrix(0, ncol = length(x.columns), nrow = sum(sample.size.matrix[, 
        1]))
    time.loc <- cumsum(sample.size.matrix[, 1])
    corr.matrix <- matrix(0, ncol = length(x.columns), nrow = length(x.columns))
    denom.j.vector <- rep(0, length = length(x.columns))
    denom.j.matrix <- matrix(0, ncol = length(x.columns), nrow = length(x.columns))
    for (tindex in 1:length(unique.times)) {
        for (j in 1:length(x.columns)) {
            the.tj.sub.yvec <- degmat[degmat[[time.column]] == 
                unique.times[tindex], x.columns[j]]
            for (k in 1:length(x.columns)) {
                the.tk.sub.yvec <- degmat[degmat[[time.column]] == 
                  unique.times[tindex], x.columns[k]]
                corr.matrix[j, k] <- corr.matrix[j, k] + sum((the.tj.sub.yvec - 
                  mean.matrix[tindex, j]) * (the.tk.sub.yvec - 
                  mean.matrix[tindex, k]))
            }
            denom.j.vector[j] <- denom.j.vector[j] + sum((the.tj.sub.yvec - 
                mean.matrix[tindex, j])^2)
            if (tindex == 1) 
                start.time <- 1
            else start.time <- time.loc[tindex - 1] + 1
            time.loc.now <- start.time:time.loc[tindex]
            plot.matrix[time.loc.now, j] <- the.tj.sub.yvec - 
                mean.matrix[tindex, j]
        }
    }
    denom.matrix <- outer(sqrt(denom.j.vector), sqrt(denom.j.vector))
    corr.matrix <- corr.matrix/denom.matrix
    dimnames(corr.matrix) <- list(x.columns, x.columns)
    cat("\n\nDegradation cdorrelation matrix\n")
    print(corr.matrix)
    dimnames(plot.matrix) <- list(rep(" ", length = nrow(plot.matrix)), 
        x.columns)
    pairs(plot.matrix)
    invisible()
}
