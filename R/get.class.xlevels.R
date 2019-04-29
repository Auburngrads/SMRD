get.class.xlevels <-
function (the.xmat, Terms, group.var = 1:ncol(the.xmat)) 
{
    if (length(group.var) > 0) {
        xlevels <- lapply(the.xmat[group.var], levels)
        xlevels <- xlevels[!sapply(xlevels, is.null)]
        if (length(xlevels) == 0) 
            xlevels <- NULL
    }
    else xlevels <- NULL
    return(xlevels)
}
