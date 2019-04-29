vector.generic.distribution <-
function (dist.vec, allow = F) 
{
    for (i in 1:length(dist.vec)) {
        dist.vec[i] <- generic.distribution(dist.vec[i], allow = allow)
    }
    return(dist.vec)
}
