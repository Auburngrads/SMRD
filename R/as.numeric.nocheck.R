as.numeric.nocheck <-
function (x,...)
{
    old.warn <- options(warn = -1)
    on.exit(options(old.warn))
    y <- as.numeric(as.character(x))
    return(y)
}
