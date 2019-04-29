log.min <-
function (x,base = exp(1))
{
    x[x <= 0] <- 1e-30
    return(logb(x))
}
