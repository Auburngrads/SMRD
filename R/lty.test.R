lty.test <-
function (x = 0:10) 
{
    plot(c(0, 1), c(0, 1), type = "n")
    for (i in 1:length(x)) {
        the.x <- c(x.loc(0.1), x.loc(0.9))
        the.y <- c(y.loc((i - 0.5)/length(x)), y.loc((i - 0.5)/length(x)))
        lines(the.x, the.y, lty = x[i])
        text(x.loc(0.95), y.loc((i - 0.5)/length(x)), as.character(x[i]))
    }
}
