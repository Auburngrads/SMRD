trim.mean.var <-
function (x, trim = 0) 
{
    x <- as.double(x)
    if (trim > 0) {
        n <- length(x)
        i1 <- floor(trim * n) + 1
        i2 <- n - i1 + 1
        x <- sort(x, unique(c(i1, i2)))[i1:i2]
    }
    return(list(mean = sum(x)/length(x), var = var(x)))
}
