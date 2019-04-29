get.the.right.stuff <-
function (data.ld, 
          column, 
          markers)
{
    the.xmat <- xmat(data.ld)
    
    for (i in 1:ncol(the.xmat)) {
      
        the.column <- the.xmat[, i]
        if (is.numeric(the.column))
            the.xmat[, i] <- signif(the.column, 4)
        
    }
    the.x.markers <- 
      vector.strip.blanks(apply(the.xmat[, column, drop = F], 
                                1, 
                                paste, 
                                collapse = ";"))
    right.stuff <- !is.na(match(the.x.markers, markers))
    
    return(right.stuff)
}
