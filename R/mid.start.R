mid.start <-
function (theta.hat, range, size) 
{
    if (theta.hat <= range[1]) 
        return(1)
    if (theta.hat >= range[2]) 
        return(2 * size + 1)
    return(floor(((theta.hat - range[1])/(range[2] - range[1])) * 
        (2 * size) + 1.01))
}
