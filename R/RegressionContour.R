RegressionContour <-
function (results.out, range.list, the.prob = 0.1, size = 3, 
    v = NULL, do.log10 = F) 
{
    param.matrix <- get.seq.mat(range.list, size = size)
    f.quantiles.groupm.out <- function(x, results.out, prob.vec) {
        x <- paste(x, collapse = ";")
        tmp <- quantiles.groupm.out(groupm.list = results.out, 
            new.data = x, prob.vec = prob.vec, printem = F)
        return(tmp[2])
    }
    zmat <- matrix(apply(param.matrix, 1, f.quantiles.groupm.out, 
        results.out = results.out, prob.vec = the.prob), ncol = size)
    theta.matrix <- attr(param.matrix, "theta.matrix")
    zmat <- matrix(apply(param.matrix, 1, f.quantiles.groupm.out, 
        results.out = results.out, prob.vec = the.prob), ncol = size)
    x.names <- names(xmat(results.out[[1]]$data.ld))
    if (do.log10) {
        the.list <- list(x = theta.matrix[, 1], y = theta.matrix[, 
            2], z = log10(zmat))
        contour(the.list, xlab = x.names[1], ylab = x.names[2])
    }
    else {
        the.list <- list(x = theta.matrix[, 1], y = theta.matrix[, 
            2], z = zmat)
        contour(the.list, xlab = x.names[1], ylab = x.names[2])
    }
    return(the.list)
}
