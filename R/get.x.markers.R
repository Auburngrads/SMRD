get.x.markers <-
function (data.ld, 
          sending.xmat = F, 
          group.var = 1:ncol(the.xmat),
          long = F, 
          collapse.on = ";", 
          do.order = T, 
          include.complete = F)
{
    `if`(long && GetSMRDDefault("SMRD.long.names"),
         long <- T,
         long <- F)
  
    `if`(sending.xmat,
         the.xmat <- data.ld,
         the.xmat <- xmat(data.ld))

    if (!missing(group.var) && any(is.na(group.var))) {
      
        return(NA)
      
    }
    
    `if`(do.order,
         order.vec <- order(the.xmat[, group.var[1]]),
         order.vec <- 1:nrow(the.xmat[, group.var, drop = F]))
    
    if (long) {
      
       for (i in 1:ncol(the.xmat)) {
      
            the.column <- the.xmat[, i]
            if (is.numeric(the.column)) {
          
               decimal.digits <- max(nchar(abs(the.column) - floor(abs(the.column))) - 2,0)
               `if`(is.integer(the.column),
                    the.xmat[, i] <- the.column,
                    the.xmat[, i] <- sprintf(fmt = paste0('%#.',decimal.digits,'f'), 
                                             the.column))          
        }   
    }
      
        the.names <- dimnames(the.xmat[, group.var, drop = F])[[2]]
        the.complete.ordered.list <- 
          apply(the.xmat[order.vec, group.var, drop = F], 
                1, 
                paste, 
                sep = "~", 
                collapse = collapse.on,
                the.names)
        
        if (include.complete)
            the.complete.list <- 
              apply(the.xmat[, group.var, drop = F], 
                    1, 
                    paste, 
                    sep = "~", 
                    collapse = collapse.on,
                    the.names)
        
        the.strings <- unique(the.complete.ordered.list)
        
  } else {
    
    for (i in 1:ncol(the.xmat)) {
      
         the.column <- the.xmat[, i]
         if (is.numeric(the.column)) {
          
             the.xmat[, i] <- signif(the.column, 4)
          
        }
    }
    
        the.names <- NULL
        the.complete.ordered.list <- 
          apply(the.xmat[order.vec, group.var, drop = F], 
                1, 
                paste, 
                sep = "", 
                collapse = collapse.on)
        
        the.strings <- unique(the.complete.ordered.list)
        
        if (include.complete)
            the.complete.list <- 
              apply(the.xmat[, group.var, drop = F], 
                    1, 
                    paste, 
                    sep = "", 
                    collapse = collapse.on)
        
        the.strings <- unique(the.complete.ordered.list)
  }
    
    the.strings <- vector.strip.blanks(the.strings)
    attr(the.strings, "the.names") <- the.names
    
    if (include.complete)
        complete.list(the.strings) <- the.complete.list
    
    return(the.strings)
}
