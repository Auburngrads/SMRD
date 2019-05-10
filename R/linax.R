linax <-
function (xmax, xmin, nint = 5, nticks = GetSMRDDefault("SMRD.NumberTicks"),...)
{
    if (missing(xmin) && length(xmax == 2)) {
        xmin <- xmax[1]
        xmax <- xmax[2]
    }
    ticlab <- wqm.pretty(c(xmax, xmin), nint = nint)
    ticloc <- seq(ticlab[1], ticlab[length(ticlab)], 
                  length = (length(ticlab) - 1) * (nticks + 1) + 1)
    
    return(list(ticlab = format(ticlab), ticloc = format(ticloc)))
}
