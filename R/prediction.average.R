prediction.average <-
function (timevec, mu, sigma, distribution) 
{
    zout <- .Fortran("postpr", as.double(timevec), as.integer(length(timevec)), 
        as.double(mu), as.double(sigma), as.integer(length(mu)), 
        as.integer(numdist(distribution)), pdf = double(length(timevec)), 
        cdf = double(length(timevec)))
    return(list(x = timevec, post.pred.den = zout$pdf, post.pred.cdf = zout$cdf))
}
