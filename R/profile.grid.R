profile.grid <-
function (fitted, which,...)
{
    if (which == "x") {
        the.dim <- 1
        x <- fitted$x
        xlab <- fitted$xlab
    }
    else {
        the.dim <- 2
        x <- fitted$y
        xlab <- fitted$ylab
    }
    struct1 <- list(y = apply(fitted$z, the.dim, max), x = x, xlab = xlab,
        subtitle = fitted$subtitle, distribution = fitted$distribution,
        form = fitted$form, number.parameters = fitted$number.parameters)
    return(struct1)
}
