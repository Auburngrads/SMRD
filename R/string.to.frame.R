string.to.frame <-
function (the.string, col.names = NULL) 
{
    if (is.data.frame(the.string))  return(the.string)
    hold.options <- options(warn = -1)
    on.exit(options(hold.options))
    
    if (length(the.string) > 1) {
        print(the.string)
        stop("the.string is a vector")
        
    }
    the.string <- my.strip.blanks(the.string)
    if (is.null(the.string) || nchar(the.string) <= 0 || the.string == "None") return(NULL)
    the.semi.vector <- ClistToVec(the.string, sep = ",")
    the.semi.vector <- the.semi.vector[nchar(the.semi.vector) > 0]
    the.number.of.columns <- length(ClistToVec(the.semi.vector[1], ";"))
    the.char.mat <- matrix(NA, 
                           nrow = length(the.semi.vector), 
                           ncol = the.number.of.columns)
    
    for (i in 1:length(the.semi.vector)) {
        
        the.row <- ClistToVec(the.semi.vector[i], ";")
        if (length(the.row) != the.number.of.columns) stop(paste("String", i, 
                                                                 the.semi.vector[i],
                                                                 "not consistent with string 1 having length", 
                                                                 the.number.of.columns))
        the.char.mat[i, ] <- ClistToVec(the.semi.vector[i], ";")
        
    }
    the.frame <- as.data.frame(the.char.mat)
    
    for (j in 1:ncol(the.frame)) {
        
        the.column <- the.char.mat[, j]
        the.column.numeric <- as.numeric(the.column)
        `if`(all(!is.na(the.column.numeric)),
             the.frame[[j]] <- the.column.numeric,
             the.frame[[j]] <- factor(as.character(the.column)))
        
    }
    
    if (!is.null(col.names)) names(the.frame) <- col.names
    return(the.frame)
    
}
