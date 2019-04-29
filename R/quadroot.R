quadroot <-
function (a, b, c) 
{
    discrim <- b^2 - 4 * a * c
    if (discrim < 0) {
        stop("neg discrim")
    }
    return((-b + sqrt(discrim))/(2 * a))
}
