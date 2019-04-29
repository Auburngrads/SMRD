times <-
function (data.d)
{
    time.column <- get.time.column(data.d)
    if (is.null(time.column)) {
      
        time.column <- attr(data.d, "times.column")
        
    }
    
    if (is.null(time.column)) {
      
        the.times <- data.d$times
        `if`(!is.null(the.times),
             return(the.times),
             stop("Internal error SMRD---cannot find times"))
        
    }
    
    the.times <- as.matrix(data.d[, time.column, drop = F])
    col.names <- dimnames(data.d)[[2]]
    names(col.names) <- col.names
    dimnames(the.times)[[2]] <- col.names[time.column]
    
    return(the.times)
}


`times<-` <-
    function (data.ld, value)
    {
      value <- as.matrix(value)
      old.times <- times(data.ld)
      if (ncol(value) == ncol(old.times)) {
        times.names <- dimnames(old.times)[[2]]
      } else {
        if (ncol(value) == 2 && ncol(old.times) == 1) {
          times.names <- paste(c(dimnames(old.times)[[2]],
                                 dimnames(old.times)[[2]]), c("L", "U"), sep = "")
        } else {
          if (ncol(value) == 1 && ncol(old.times) == 1)
            times.names <- dimnames(old.times)[[2]][1]
          else stop(paste("Bad times new col=", ncol(value),
                          "and old col=", ncol(old.times)))
        }
        dimnames(value) <- list(as.character(1:nrow(value)),
                                times.names)
      }
      times.length <- nrow(value)
      if (times.length != nrow(data.ld))
        stop(paste("Inserting times  with length", times.length,
                   "but data objects had", nrow(data.ld), "rows"))
      time.columns <- attr(data.ld, "time.column")
      all.attributes <- attributes(data.ld)
      frame.names <- names(data.ld)
      time.col.numbers <- match(names(data.ld[, time.columns, drop = F]),
                                frame.names)
      new.frame <- data.frame(data.ld[, -time.col.numbers, drop = F],
                              value)
      all.attributes$time.column <- dimnames(value)[[2]]
      all.attributes$names <- names(new.frame)
      attributes(new.frame) <- all.attributes
      if (map.SMRDDebugLevel() >= 6)
        cix(new.frame)
      return(new.frame)
    }