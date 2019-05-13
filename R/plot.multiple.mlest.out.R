#' @export
plot.multiple.mlest.out <-
function (x, index = 1,...)
{
    old.par <- par(mar = c(5.1, 5.1, 4.1, 2.1))
    the.groups <- as.numeric(names(x))
    plot.vector <- rep(NA, length(the.groups))
    for (i in 1:length(the.groups)) {
        plot.vector[i] <- x[[i]]$theta.hat[index]
    }
    the.order <- order(the.groups)
    plot(the.groups[the.order], plot.vector[the.order], ylab = "",
        xlab = "", cex = 1.5)
    title(xlab = "Group Number", ylab = as.character(index),
        cex = 1.5)
}
