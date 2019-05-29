make.frame.ld <-
function (y, 
          the.censor.codes = NULL, 
          the.case.weights = NULL,
          the.truncation.codes = NULL, 
          ty = NULL, 
          the.failure.modes = NULL,
          time.units = NULL, 
          the.xmat = NULL, 
          xlabel = NULL, 
          data.title = "No title",
          data.note = "")
{
    y <- as.matrix(y)
    
    if (is.null(time.units)) time.units <- "Time"
    
    `if`(ncol(y) == 2,
         dimnames(y) <- list(rep(NULL, nrow(y)), 
                            paste(c("lower", "upper"), time.units)),
         dimnames(y) <- list(rep(NULL, nrow(y)), time.units))
    
    response.column <- vector.strip.blanks(dimnames(y)[[2]], 
                                           FillChar = ".")
    
    if (length(the.censor.codes) > 0) {
      
        if (all(the.censor.codes == 1)) {
          
            censor.column <- NULL
            
      } else {
        
            the.censor.codes <- as.matrix(the.censor.codes)
            dimnames(the.censor.codes) <- list(rep(NULL, nrow(the.censor.codes)), "Status")
            censor.column <- "Status"
            
      }
      
  } else {
    
    censor.column <- NULL
    
  }
    if (length(the.case.weights) > 0) {
      
        if (all(the.case.weights == 1)) {
          
            case.weight.column <- NULL
            
      } else {
        
            the.case.weights <- as.matrix(the.case.weights)
            dimnames(the.case.weights) <- list(rep(NULL, nrow(the.case.weights)),"Weights")
            case.weight.column <- "Weights"
            
      }
      
  } else {
    
    case.weight.column <- NULL
    
  }
    if (length(the.xmat) > 0) {
      
        the.xmat <- as.data.frame(the.xmat)
        x.columns <- dimnames(the.xmat)[[2]]
        
        } else {
          
        x.columns <- NULL
    
        }
    
    if (length(the.failure.modes) > 0) {
      
        the.failure.modes <- as.matrix(the.failure.modes)
        failure.mode.column <- dimnames(the.failure.modes)[[2]]
        
        } else {
          
        failure.mode.column <- NULL
  
        }
    
    if (length(ty) > 0) {
      
        if (length(the.truncation.codes) == 0) stop("Truncation times given withour truncation codes")
      
        ty <- as.matrix(ty)
        
        if (ncol(ty) == 2) {
          
            dimnames(ty) <- list(rep(NULL, nrow(ty)), paste(c("lower", "upper"), "Trun"))
            
        } else {
          
            dimnames(ty) <- list(rep(NULL, nrow(ty)), "Trun")
            
        }
        
        truncation.response.column <- dimnames(ty)[[2]]
        the.truncation.codes <- as.matrix(the.truncation.codes)
        dimnames(the.truncation.codes) <- list(rep(NULL, nrow(the.truncation.codes)), "TrunCode")
        truncation.type.column <- "TrunCode"
        
    } else {
      
        if (length(the.truncation.codes) > 0) stop("Truncation codes given withour truncation times")
        truncation.type.column <- NULL
        truncation.response.column <- NULL
        
    }   
    
    the.frame <- my.data.frame(y, 
                               the.censor.codes, 
                               the.case.weights,
                               the.failure.modes, 
                               the.xmat, 
                               the.truncation.codes, 
                               ty)
    
    the.frame.ld <- 
      frame.to.ld(the.frame, 
                  response.column = response.column,
                  censor.column = censor.column, 
                  case.weight.column = case.weight.column,
                  failure.mode.column = failure.mode.column, 
                  x.columns = x.columns,
                  truncation.response.column = truncation.response.column,
                  truncation.type.column = truncation.type.column, 
                  time.units = time.units, 
                  data.title = data.title, 
                  data.note = data.note, 
                  xlabel = xlabel)
    
    invisible(the.frame.ld)
}
