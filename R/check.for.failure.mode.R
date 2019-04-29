check.for.failure.mode <-
function (data.mfld, fmode) 
{
    the.names <- names(data.mfld)
    if (is.numeric(fmode)) {
        if (fmode < 0) 
            stop("fmode is less than 0")
        fmode <- fmode + 1
        if (fmode > length(data.mfld)) 
            stop("fmode is larger than the number of modes")
    }
    else {
        if (!is.character(fmode)) 
            stop("fmode must be either numeric or character")
        if (is.onlist(fmode, the.names)) 
            stop("fmode not a recognized failure mode")
    }
    return(fmode)
}
