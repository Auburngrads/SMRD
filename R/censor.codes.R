censor.codes <-
function (data.d, fill.in = T, warn.all.ones = F)
{
    frame.type <- data.object.type(data.d)
    
    switch(frame.type[1], frame.centered = {
      
        the.censor.status <- censor.status(data.d)
        
        if (is.null(the.censor.status)) {
          
            `if`(fill.in, 
                 the.censor.codes <- rep(1, length = nrow(Response(data.d))),
                 return(NULL))
            
          } else { 
          
            the.censor.codes <- 
              ConvertToCensorCodes(the.censor.status,
                                   failure.censor.names = get.failure.censor.names(data.d),
                                   right.censor.names = get.right.censor.names(data.d),
                                   left.censor.names = get.left.censor.names(data.d),
                                   interval.censor.names = get.interval.censor.names(data.d),
                                   sinterval.censor.names = get.sinterval.censor.names(data.d),
                                   warn.all.ones = warn.all.ones)
          
          }
        
    }, list.centered = {
      
        censor.column <- data.d$censor.column
        
        if (is.null(censor.column)) { 
          
          the.censor.codes <- rep(1, length = nrow(Response(data.d)))
        
        }
        
        the.censor.codes <- data.d$frame[, censor.column]
        
    }, unfolded = {
      
        if (any(names(data.d) == "censor.codes") && !is.null(data.d$censor.codes)) the.censor.codes <- data.d$censor.codes else the.censor.codes <- rep(1,
            length = nrow(Response(data.d)))
    }, {
        stop("Corrupted data frame")
    })
    
    if (is.null(the.censor.codes))
        stop("Null censor.codes")
    return(the.censor.codes)
}

#
#

`censor.codes<-` <-
  function (data.ld, value) 
  {
    if (is.null(value)) {
      attr(data.ld, "censor.column") <- NULL
      attr(data.ld, "censor.codes") <- NULL
      return(data.ld)
    }
    new.censor.codes.length <- length(value)
    if (new.censor.codes.length != nrow(data.ld)) 
      stop(paste("Inserting censor.codes  with length", new.censor.codes.length, 
                 "but data objects had", nrow(data.ld), "rows"))
    the.censor.codes.columns <- attr(data.ld, "censor.column")
    the.old.censor.codes <- censor.codes(data.ld, fill.in = F)
    frame.names <- names(data.ld)
    if (new.censor.codes.length == length(the.old.censor.codes)) {
      data.ld[, the.censor.codes.columns] <- value
      attr(data.ld, "censor.codes") <- value
      return(data.ld)
    } else {
      censor.codes.names <- "InsCensor"
      all.attributes <- attributes(data.ld)
      frame.names <- names(data.ld)
      new.frame <- data.frame(data.ld, InsCensor = value)
      all.attributes$censor.column <- censor.codes.names
      all.attributes$names <- names(new.frame)
      attributes(new.frame) <- all.attributes
      attr(new.frame, "censor.codes") <- value
      return(new.frame)
    }
  }
