mono.lower <-
function (lower) 
{
    zout <- .Fortran("bfixl", lower = as.single(lower), length(lower))
    return(zout$lower)
}
