TranTime <-
function (time, distribution) 
{
    if (is.logdist(distribution)) 
        return(logb(time))
    else return(time)
}
