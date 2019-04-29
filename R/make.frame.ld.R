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
    response.column <- as.matrix(y)
    
    if (is.null(time.units)) time.units <- "Time"
    
    if (ncol(response.column) == 2) {
      
        colnames(response.column) <- paste(c("lower","upper"), time.units)
  
        } else {
          
        colnames(response.column) <- time.units
    }
    
    if (length(the.censor.codes) > 0) {
      
        if (all(the.censor.codes == 1)) {
          
            censor.column <- NULL
            
      } else {
        
            censor.column <- as.matrix(the.censor.codes)
            colnames(censor.column) <- "Status"
      }
      
  } else {
    
    censor.column <- NULL
    
  }
    if (length(the.case.weights) > 0) {
      
        if (all(the.case.weights == 1)) {
          
            case.weight.column <- NULL
            
      } else {
        
            case.weight.column <- as.matrix(the.case.weights)
            colnames(case.weight.column) <- "Weights"
      }
      
  } else {
    
    case.weight.column <- NULL
    
  }
    if (length(the.xmat) > 0) {
      
        x.columns <- as.data.frame(the.xmat)
        colnames(x.columns) <- dimnames(the.xmat)[[2]]
        
        } else {
          
        x.columns <- NULL
    
        }
    
    if (length(the.failure.modes) > 0) {
      
        failure.mode.column <- as.matrix(the.failure.modes)
        colnames(failure.mode.column) <- "Mode"
        
        } else {
          
        failure.mode.column <- NULL
  
        }
    
    if (length(ty) > 0) {
      
        if (length(the.truncation.codes) == 0)
            stop("Truncation times given withour truncation codes")
      
        truncation.response.column <- as.matrix(ty)
        
        if (ncol(ty) == 2) {
          
            colnames(truncation.response.column) <- paste(c("lower","upper"), "Trun")
      
            } else {
              
            colnames(truncation.response.column) <- "Trun"
        
            }
        
        truncation.type.column <- as.matrix(the.truncation.codes)
        colnames(truncation.type.column) <- "TrunCode"
  
        } else {
          
        if (length(the.truncation.codes) > 0)
            stop("Truncation codes given withour truncation times")
        truncation.type.column <- NULL
        truncation.response.column <- NULL
  
        }
    
    the.frame <- my.data.frame(y, 
                               censor.column, 
                               case.weight.column,
                               failure.mode.column, 
                               x.columns, 
                               truncation.response.column, 
                               truncation.type.column)
    
    the.frame.ld <- 
      frame.to.ld(the.frame, 
                  response.column = colnames(response.column),
                  censor.column = colnames(censor.column), 
                  case.weight.column = colnames(case.weight.column),
                  failure.mode.column = colnames(failure.mode.column), 
                  x.columns = x.columns,
                  truncation.response.column = colnames(truncation.response.column),
                  truncation.type.column = colnames(truncation.type.column), 
                  time.units = time.units, 
                  data.title = data.title, 
                  data.note = data.note, 
                  xlabel = xlabel)
    
    invisible(the.frame.ld)
}
