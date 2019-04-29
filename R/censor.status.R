censor.status <-
function (data.d) 
{
    if (is.character(data.d) && length(data.d) == 1) 
        data.d <- checkget(data.d)
    censor.column <- attr(data.d, "censor.column")
    
    if (is.null(censor.column)) {
      
        the.status <- NULL
        
    } else {
      
        the.status <- data.d[[censor.column]]
        attr(the.status, "from.multiple.failure.mode") <- F
    }
    
    the.failure.modes <- failure.modes(data.d)
    if (is.null(the.status) && !is.null(the.failure.modes)) {
        the.status <- as.character(the.failure.modes)
        the.censor.codes <- ConvertToCensorCodes(the.failure.modes, 
            warn.unrecognized = F, warn.all.ones = F)
        the.status[is.na(the.censor.codes)] <- "Failed"
        the.status <- factor(the.status)
        attr(the.status, "from.multiple.failure.mode") <- T
    }
    return(the.status)
}
