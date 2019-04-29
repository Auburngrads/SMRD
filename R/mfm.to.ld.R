mfm.to.ld <-
function (data.ld, 
          modes = NULL, 
          prefix = NULL, 
          allow.poor.data = T,
          do.assign = T)
{
    if (is.null(failure.modes(data.ld)))
        stop("Input must be a life data object with failure modes defined")
  
    if (is.null(prefix)) prefix <- GetDataPrefix(deparse(substitute(data.ld)))
    
    data.mfld <- ld.to.mfld(data.ld)
    
    if (is.null(modes)) modes <- names(data.mfld)
    
    for (i in 1:length(modes)) {
      
        the.ld.name <- paste(prefix, 
                             ".", 
                             strip.blanks.nulls(modes[i]),
                             ".ld", 
                             sep = "")
        
        poor.data <- !good.data(data.mfld[[modes[i]]], number.needed = 2)
        
        if (do.assign && (!poor.data || allow.poor.data)) {
          
            assign(envir = .frame0,
                   inherits = TRUE,
                   the.ld.name, 
                   data.mfld[[modes[i]]])
        }
        
        if (poor.data) {
            warning(paste("Fewer than two failures of type", modes[i]))
        }
    }
    return(data.mfld)
}
