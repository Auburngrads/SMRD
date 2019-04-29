wmean <-
function (x, weights = rep(1, length(x))) 
{
    return(sum(x * weights)/sum(weights))
}
