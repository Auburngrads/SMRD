prediction.order.average <-
function (korder, 
          nsamsize, 
          timevec, 
          mu, 
          sigma, 
          distribution) 
{
    zout <- POSTKP(as.integer(korder), 
                   as.integer(nsamsize), 
                   as.double(timevec),
                   as.integer(length(timevec)), 
                   as.double(mu), 
                   as.double(sigma),
                   as.integer(length(mu)), 
                   as.integer(numdist(distribution)), 
                   pdf = double(length(timevec)), 
                   cdf = double(length(timevec)))
    
    return(list(x = timevec, 
                post.pred.den = zout$pdf, 
                post.pred.cdf = zout$cdf))
}
