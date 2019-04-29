ld.to.mfld <-
function (data.ld, 
          number.needed = 2, 
          censored.in.interval = "average")
{
    the.failure.modes <- SMRD:::failure.modes(data.ld)
    if (is.null(the.failure.modes))
        stop("Data set does not have failure modes defined")
    
    unique.modes <- as.character(unique(the.failure.modes))
    unique.modes <- 
      sort(unique.modes[is.na(match(casefold(unique.modes),
        SMRD:::ClistToVec(SMRD:::get.right.censor.names(data.ld))))])
    multiple.d.list <- list()
    for (i in 1:length(unique.modes)) {
        y <- Response(data.ld)
        the.censor.codes <- SMRD:::censor.codes(data.ld)
        the.case.weights <- SMRD:::case.weights(data.ld)
        the.truncation.codes<- SMRD:::truncation.codes(data.ld)
        ty <- NULL
        this.one <- the.failure.modes == unique.modes[i]
        
        if (length(the.failure.modes[this.one]) < number.needed) {
          
            cat(paste("\n***Skipping mode", unique.modes[i],
                "because only ", length(the.failure.modes[this.one]),
                "failures of that type\n"))
            next
        }
        
        the.censor.codes[!this.one] <- 2
        
        if (ncol(y) == 2 && any(!this.one)) {
          
            y[!this.one, c(1, 2)] <- 
              switch(censored.in.interval, 
                     average    = { (y[!this.one, 1] + y[!this.one, 2])/2 }, 
                     logaverage = { exp((logb(y[!this.one, 1]) + logb(y[!this.one, 2]))/2) }, 
                     upper      = { y[!this.one, 2] }, 
                     lower      = { y[!this.one, 1] })
        }
        data.title <- paste(SMRD:::get.data.title(data.ld), 
                            unique.modes[i],
                            "Failure Mode")
        time.units = as.character(colnames(Response(data.ld)))
        the.xmat = xmat(data.ld)
        xlabel = SMRD:::get.xlabel(data.ld)
        xx.ld <- SMRD:::make.frame.ld(y = y, 
                               the.censor.codes = the.censor.codes,
                               the.case.weights = the.case.weights, 
                               data.title = data.title,
                               time.units = time.units, 
                               the.xmat = the.xmat,
                               xlabel = xlabel, 
                               the.truncation.codes = the.truncation.codes, 
                               ty = NULL)
        
        multiple.d.list[[unique.modes[i]]] <- xx.ld
    }
    oldClass(multiple.d.list) <- "multiple.failure.mode.life.data"
    attr(multiple.d.list, "data.ld") <- data.ld
    return(multiple.d.list)
}
