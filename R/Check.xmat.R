Check.xmat <-
function (the.xmat, explan.vars, number.cases) 
{
    if (is.null(the.xmat)) 
        stop("Explanatory variables requested, but there is no X matrix")
    stop.message <- "Non numeric explanatory variables not allowed at this level"
    
    if (is.matrix(the.xmat)) {
        if (is.data.frame(the.xmat)) the.xmat <- as.matrix(the.xmat)
        if (!is.numeric(the.xmat))   stop(stop.message)
        
    } else {
      
        if (!any(unlist(lapply(the.xmat, is.numeric)))) 
            stop(stop.message)
    }
    the.xmat <- as.matrix(the.xmat)
    
    if (nrow(the.xmat) != number.cases) 
        stop(paste("Number of rows in x matrix ", 
                   nrow(the.xmat), 
                   " is wrong"))
    if (any(explan.vars <= 0)) 
        stop("Negative or 0 explanatory variables column specified")
    if (max(explan.vars) > ncol(the.xmat)) 
        stop("Specified explanatory variable column greater than number of columns in X matrix")
    return(the.xmat)
}
