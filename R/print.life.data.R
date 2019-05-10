print.life.data <-
function (x, 
          includex = T, 
          quote = F, 
          prefix = "", 
          digits = 4,...)
{
    cat(paste("Data from ", get.data.title(x), "\n"))
    obs.type <- c("Dummy", 
                  "Failure", 
                  "R-Censored", 
                  "L-Censored",
                  "Interval", 
                  "Small-Interval")
    
    trun.type <- c("None", "Right", "Left", "Interval")
    the.case.weights <- case.weights(x)
    the.censor.codes <- censor.codes(x, fill.in = F)
    
    if(is.null(the.case.weights) || all(the.case.weights == 1)) {
      
       name.case.weights <- NULL
       the.case.weights <- NULL
        
     } else {
    
       name.case.weights <- "Case.weights"
    
     }
    
    if(is.null(failure.modes(x))) {
      
       name.fail.modes <- NULL
       the.failure.modes <- NULL
       
     } else {
       
       name.fail.modes <- "Failure.Modes"
       the.failure.modes <- as.character(failure.modes(x))
       
     }
    
    if(is.null(the.censor.codes) || all(the.censor.codes == 1)) {
      
       name.censor.code <- NULL
       status <- NULL
       if(all(the.censor.codes == 1)) the.censor.codes <- NULL
       
     } else {
       
       name.censor.code <- "Status"
       status.ind <- match(censor.codes(x), 0:5)
       status <- obs.type[status.ind]
       
     }
    
    if(is.null(truncation.codes(x))) {
      
       name.truncation.code <- NULL
       name.truncation.resp <- NULL
       trunc.status <- NULL
        
     } else {
       
       name.truncation.code <- "TrunCode"
       name.truncation.resp <- colnames(truncation.response(x))
       trunc.ind <- match(truncation.codes(x), 1:4)
       trunc.status <- trun.type[trunc.ind]
       
     }
    
    the.frame <- data.frame(cbind(Response(x), 
                                  the.censor.codes,
                                  the.case.weights, 
                                  the.failure.modes, 
                                  trunc.status, 
                                  truncation.response(x)))
    
    if(!is.null(xmat(x))) {
      
       if(dimnames(xmat(x))[[2]][1] == "(Intercept)") {
         
          xmat(x) <- xmat(x)[, -1, drop = F]
          
       }
      
       the.frame <- data.frame(the.frame, xmat(x))
       
    }
    
    names(the.frame) <- c(colnames(Response(x)), 
                          name.censor.code,
                          name.case.weights, 
                          name.fail.modes, 
                          name.truncation.code,
                          name.truncation.resp, 
                          dimnames(xmat(x))[[2]])
    
    the.frame$Status <- status
    print.data.frame(the.frame)
    invisible(the.frame)
    
}
