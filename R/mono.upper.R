mono.upper <-
function (upper) 
{
    zout <- BFIXU(as.double(upper))
    
    return(zout$upper)
    
}
