trim.vector <-
function (x, trim = 0) 
{
    if (trim > 0) {
        if (trim >= 0.5) 
            stop("cannot trim more than 50%")
        n <- length(x)
        i1 <- floor(trim * n) + 1
        i2 <- n - i1 + 1
        x <- sort(x, partial = unique(c(i1, i2)))[i1:i2]
    }
    return(x)
}
