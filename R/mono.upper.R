mono.upper <-
function (upper) 
{
    zout <- bfixu(as.double(upper))
    
    return(zout$upper)
    
}
