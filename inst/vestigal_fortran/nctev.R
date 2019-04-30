nctev <-
function (ndf, del, t) 
{
    number <- max(length(ndf), length(del), length(t))
    ndf <- expand.vec(ndf, number)
    del <- expand.vec(del, number)
    t <- expand.vec(t, number)
    zout <- .Fortran("snctev", ndf = as.integer(ndf), del = as.double(del), 
        t = as.double(t), number = as.integer(number), ans = double(number), 
        der = double(number))
    ans <- zout$ans
    attr(ans, "derivative") <- zout$der
    return(ans)
}
