check.column <-
function (input.column, 
          ncol.data.mat, 
          names.the.frame, 
          number.col.allowed = 1)
{
    input.name <- deparse(substitute(input.column))
    
    if (length(number.col.allowed) > 1 || number.col.allowed != -1) {
      
        if (length(number.col.allowed) == 1) {
          
            if (length(input.column) != number.col.allowed) stop(paste(input.name, 
                                                                       number.col.allowed))
      } else {
        
            if (length(input.column) < min(number.col.allowed) | length(input.column) > max(number.col.allowed)) {
              
                stop(paste(input.name, 
                           "should be between",
                           number.col.allowed[1], 
                           "and", number.col.allowed[2]))
              
            }
        }
    }
    
    if (is.numeric(input.column)) {
      
        if (any(input.column > ncol.data.mat)) stop(paste(input.name, 
                                                          paste(input.column, collapse = ", "),
                                                          " larger than number of data columns = ", 
                                                          ncol.data.mat))
        input.column <- names.the.frame[input.column]
        
 } else {
   
        for (i in 1:length(input.column)) {
          
            if (!is.onlist(input.column[i], names.the.frame)) {
              
                cat("Column names in the current data set:\n")
                print(names.the.frame)
                stop(paste(names(input.column[i]), "is not in the input frame"))
                
            }
        }
    }
    if (any(duplicated(input.column))) {
      
        stop(paste("Duplicates in", 
                   paste(input.name, collapse = ","),
                   paste(input.column, collapse = ", ")))
      
    }
    
    return(input.column)
    
}
