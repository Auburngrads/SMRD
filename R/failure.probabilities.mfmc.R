#' @export
failure.probabilities.mfmc <-
function (x, 
          time.vector = NULL, 
          my.title = NULL,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          digits = GetSMRDDefault("SMRD.DigitsPrinted"),
          band.type = "pointwise", 
          add.title = NULL, 
          mono.tran = T,...)
{
    band.type <- casefold(band.type)
    data.ld <- attr(x, "data.ld")
    
    if (is.null(time.vector))
        time.vector <- get.time.vector(data.ld, 
                                              distribution = x[[1]]$distribution,
                                              number = 10)
    
    distribution.vec <- unlist(lapply(x, function(sublist) {
        sublist[["distribution"]]
    }))
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    
    if (length(unique(distribution.vec)) > 1) {
      
        distribution <- cbind(names(x), distribution.vec)
        dimnames(distribution) <- list(rep("", length = nrow(distribution)),
            c("Failure Mode", "Distribution"))
        
        } else {
          
        distribution <- distribution.vec[1]
    
        }
    
    conf.char <- percent.conf.level(conf.level)
    conf.int.title <- paste("Pointwise Approximate", 
                            conf.char, 
                            "Confidence Intervals")
    
    my.title <- paste("Combined Multiple Failure Mode Series System\nParametric ML CDF Estimates from ",
                       get.data.title(data.ld), 
                      add.title, "\n\n", 
                      conf.int.title,
                      "\n", 
                      sep = "")
    
    if (band.type == "pointwise") {
      
        mfmc.probs.out <- f.mfmc.probs(multiple.mlest.out = x, 
                                              time.vec = time.vector)
        the.list <- 
          compute.confidence.interval(mfmc.probs.out$vec,
                                      mfmc.probs.out$se, 
                                      kodet = rep(3, length(mfmc.probs.out$vec)),
                                      conf.level = conf.level)
        hold.list <- the.list
        hold.tme.vector <- time.vector
        the.test <- the.list$fun.lower + the.list$fun.upper
        the.list$fun.hat   <- strip.na(the.list$fun.hat, the.test)
        the.list$fun.lower <- strip.na(the.list$fun.lower, the.test)
        the.list$fun.upper <- strip.na(the.list$fun.upper, the.test)
        the.list$se.fun    <- strip.na(the.list$se.fun, the.test)
        time.vector        <- strip.na(time.vector, the.test)
        
        if (mono.tran) {
            if (length(time.vector) > 0) {
                the.order <- order(time.vector)
                the.list$fun.lower[the.order] <- mono.lower(the.list$fun.lower[the.order])
                the.list$fun.upper[the.order] <- mono.upper(the.list$fun.upper[the.order])
            }
            else {
            }
        }
        
    
        } else {
          
        mfmc.probs.out <- f.mfmc.probs(x, time.vector, do.se = F)
        the.list <- list(fun.hat = mfmc.probs.out)
        
        }
    
    the.table <- cbind(Times = time.vector, 
                       Fhat = the.list$fun.hat, 
                       Stderror = the.list$se.fun, 
                       Lower = the.list$fun.lower, 
                       Upper = the.list$fun.upper)
    
    if (is.null(the.table) || length(time.vector) == 0)
        return(NULL)
    
    if (ncol(the.table) == 5) {
      
        dimnames(the.table) <- 
          list(rep(" ", nrow(the.table)),
               c(get.time.units(data.ld), "Fhat", "Std.Err.", paste(conf.char, "Lower"), 
                paste(conf.char, "Upper")))
        
        } else {
      
        dimnames(the.table) <- 
          list(rep(" ", nrow(the.table)),
               c(get.time.units(data.ld), "Fhat"))
        
        }
    
    attr(the.table, "title") <- my.title
    attr(the.table, "distribution") <- distribution
    attr(the.table, "mlest.out") <- x
    oldClass(the.table) <- c("failure.probabilities.out", 
                             "estimates.out", 
                             "matrix")
    
    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}
