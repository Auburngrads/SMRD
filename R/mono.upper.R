mono.upper <-
function (upper) 
{
    zout <- .Fortran("bfixu", upper = as.single(upper), length(upper))
    return(zout$upper)
}
