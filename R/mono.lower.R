mono.lower <-
function (lower) 
{
    zout <- BFIXL(as.double(lower))
    
    return(zout$lower)
    
}
