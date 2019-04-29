`TruncationResponse<-` <-
    function (data.ld, value)
    {
      value <- as.matrix(value)
      the.old.TruncationResponse <- truncation.response(data.ld)
      
      if (ncol(value) == ncol(the.old.TruncationResponse)) {
        
        TruncationResponse.names <- dimnames(the.old.TruncationResponse)[[2]]
        
    } else {
      
        if (ncol(value) == 2 && ncol(the.old.TruncationResponse) == 1) {
          
          TruncationResponse.names <- 
            paste(c(dimnames(the.old.TruncationResponse)[[2]],
                    dimnames(the.old.TruncationResponse)[[2]]), c("L", "U"), sep = "")
          
      } else {
        
          `if`(ncol(value) == 1 && ncol(the.old.TruncationResponse) == 1,
               TruncationResponse.names <- dimnames(the.old.TruncationResponse)[[2]][1],
               stop(paste("Bad TruncationResponse new col =",
                          ncol(value), 
                          "and old col =", 
                          ncol(the.old.TruncationResponse))))
        
      }
      
        dimnames(value) <- 
          list(as.character(1:nrow(value)), TruncationResponse.names)
        
    }
      
      TruncationResponse.length <- nrow(value)
      
      if (TruncationResponse.length != nrow(data.ld))
        stop(paste("Inserting TruncationResponse  with length",
                   TruncationResponse.length, 
                   "but data objects had",
                   nrow(data.ld), "rows"))
      
      the.TruncationResponse.columns <- attr(data.ld, "truncation.response.column")
      all.attributes <- attributes(data.ld)
      frame.names <- names(data.ld)
      
      TruncationResponse.col.numbers <- 
        match(names(data.ld[, the.TruncationResponse.columns, drop = F]), 
              frame.names)
      
      new.frame <- 
        data.frame(data.ld[, -TruncationResponse.col.numbers, drop = F], 
                   value)
      
      all.attributes$truncation.response.column <- dimnames(value)[[2]]
      all.attributes$names <- names(new.frame)
      attributes(new.frame) <- all.attributes
      return(new.frame)
    
}

