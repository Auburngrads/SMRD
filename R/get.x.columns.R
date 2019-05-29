get.x.columns <-
function (data.d)
{
    x.columns <- colnames(xmat(data.d, allow = T))
    
    test.x.columns <- attr(data.d, "x.columns")
    
    if (!is.null(test.x.columns) && !all(x.columns == test.x.columns)) {
      
        warning("colnames(xmat(data.d)) != attr(data.d, \"x.columns\")")
      
    }
    
    return(x.columns)
}
