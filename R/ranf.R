ranf <-
function (nrand) 
{
    if (nrand > 0) {
        ranout <- .Fortran("ranfsb", as.single(1), as.integer(nrand), 
            answer = single(nrand))
        return(ranout$answer)
    }
    else {
        stop("nrand must be positive")
    }
    invisible()
}
