SMRD.sanity.life.data <-
function (x, weights.warn = F,...)
{
    is.near.integer <- function (x) { floor(x) == ceiling(x) }
    the.case.weights <- case.weights(data.d = x, fill.in = F)
    
    if (!is.null(the.case.weights) && weights.warn) {
      
        non.positive <- the.case.weights <= 0
        
        if (any(non.positive)) {
            warning("There are nonpositive .case.weights in the data.\nSuch observations could also result in misleading graphics.\nThey should probably be removed.\nThe row numbers will be printed.\n")
            print(seq(1, length(the.case.weights))[non.positive])
        }
        
        non.integer <- !is.near.integer(the.case.weights)
        
        if (any(non.integer)) {
            warning("There are noninteger .case.weights in the data.\nThis may indicate a mistake in input.\nSuch observations could also result in misleading graphics.\nThe noninteger case weight row numbers will be printed.\n")
            print(seq(1, length(the.case.weights))[non.integer])
        }
    }
    the.censor.codes <- censor.codes(data.d = x, fill.in = T)
    fatal <- F
    censor.code.max <- 5
    
    y <- Response(data.d = x)
    
    if (!is.matrix(y)) {
      
        cat("\nResponse should be a matrix\n")
        fatal <- T
        
    } else {

        ny <- ncol(y)
        if (ny > 2) {
            cat("\nResponse should have only 1 or 2 columns\n")
            fatal <- T
        }
    }
    if (ncol(y) == 2) {
        backward <- y[, 1] > y[, 2]
        if (any(backward)) {
            cat("\nIncorrect two column observation(s): response[,1] > response[,2] \nin the following rows:\n")
            illegals <- (1:nrow(y))[backward]
            print(y[backward, , drop = F])
            fatal <- T
        }
    }
    the.truncation.codes <- truncation.codes(data.d = x)
    ty <- truncation.response(data.d = x)
    if (!is.null(the.truncation.codes)) {
        if (is.null(ty))
            stop("truncation.codes specified, but no truncation times")
        nt <- ncol(ty)
        resp.outside.trunc.interval <- rep(F, nrow(y))
        left.censored <- the.censor.codes == 3 & the.truncation.codes > 1
        right.censored <- the.censor.codes == 2 & the.truncation.codes > 1
        interval.censored <- the.censor.codes == 4 & the.truncation.codes > 1
        left.truncated <- the.truncation.codes == 3
        right.truncated <- the.truncation.codes == 2
        interval.truncated <- the.truncation.codes == 4
        
        if (any(left.truncated)) {
          
            resp.outside.trunc.interval[left.truncated] <- 
              y[left.truncated,  1] < ty[left.truncated, 1] | 
              y[left.truncated, ny] < ty[left.truncated, 1]
            
            if (any(resp.outside.trunc.interval)) {
              
                cat("\nResponse values less than left truncation point\n")
                print(data.frame(y, 
                                 the.censor.codes, 
                                 ty, 
                                 the.truncation.codes)[resp.outside.trunc.interval, ])
                fatal <- T
            }
        }
        if (any(right.truncated)) {
            resp.outside.trunc.interval[right.truncated] <- 
              y[right.truncated,  1] > ty[right.truncated, 1] | 
              y[right.truncated, ny] > ty[right.truncated, 1]
            
            if (any(resp.outside.trunc.interval)) {
              
                cat("\nResponse values greater than right truncation point\n")
                print(data.frame(y, 
                                 the.censor.codes, 
                                 ty, 
                                 the.truncation.codes)[resp.outside.trunc.interval, ])
                fatal <- T
            }
        }
        if (any(interval.truncated)) {
            resp.outside.trunc.interval[interval.truncated] <- 
              y[interval.truncated,  1] < ty[interval.truncated, 1] | 
              y[interval.truncated, ny] > ty[interval.truncated, 2]
            
            if (any(resp.outside.trunc.interval)) {
                cat("\nResponse values outside of the truncation interval\n")
                print(data.frame(y, 
                                 the.censor.codes, 
                                 ty, 
                                 the.truncation.codes)[resp.outside.trunc.interval, ])
                fatal <- T
            }
        }
        
        } else {
          
        if (!is.null(ty))
            stop("truncation times specified, but no truncation.codes")
    }
    the.failure.modes <- failure.modes(data.d = x)
    the.censor.codes <- censor.codes(data.d = x, fill.in = F)
    
    if (!is.null(the.censor.codes)) {
      
        if (!is.null(the.failure.modes)) {
          
            the.char.failure.modes <- as.character(the.failure.modes)
            censored.by.the.modes <- 
              !is.na(match(casefold(the.char.failure.modes),
                           ClistToVec(get.right.censor.names(data.d = x))))
            problems <- 
              (censored.by.the.modes & the.censor.codes != 2) | 
              (!censored.by.the.modes & the.censor.codes == 2)
            
            if (any(problems)) {
              
                cat("\n Conflict between censor mode indicator\n and failure mode indicator in the following rows\n")
                print(data.frame(the.char.failure.modes, 
                                 the.censor.codes)[problems, ])
                fatal <- T
            }
        }
        exact.observations <- the.censor.codes == 1
        if (any(exact.observations)) {
          
            if (ncol(y) == 2) {
              
                ambiguous <- (y[, 1] != y[, 2]) & exact.observations
                
                if (any(ambiguous)) {
                  
                  cat("\n Exact observation with two unequal times\n")
                  print(cbind(y[ambiguous, , drop = F], 
                              censor = the.censor.codes[ambiguous]))
                  fatal <- T
                  
                }
            }
        }
        if (length(the.censor.codes) != nrow(y)) {
          
            cat("\nLength of censor.codes not equal to\n the number of rows in response",
                length(the.censor.codes), nrow(y))
            fatal <- T
        }
        
        illegal.censor.codes <- 
          the.censor.codes > censor.code.max | the.censor.codes < 0
        
        if (any(illegal.censor.codes)) {
          
            cat(paste("\nCensor codes illegal in the \nfollowing rows (row number over code)\n"))
            illegals <- the.censor.codes[illegal.censor.codes]
            print(illegals)
            fatal <- T
            
        }
        
        interval.censored <- the.censor.codes == 4
        
        if (any(interval.censored)) {
          
            if (ncol(y) != 2) {
                cat("\nInterval observations, but only one column\n of responses has been given; rows are:\n")
                print((1:length(the.censor.codes))[interval.censored])
                fatal <- T
            }
            resp.interval.backwards <- rep(F, nrow(y))
            resp.interval.backwards[interval.censored] <- y[interval.censored,
                1] >= y[interval.censored, ny]
            
            if (any(resp.interval.backwards)) {
              
                cat("\n Invalid interval observation(s)\n")
                print(data.frame(y, the.censor.codes)[resp.interval.backwards, ])
                fatal <- T
            }
        }
        rlcen.observations <- the.censor.codes == 2 | the.censor.codes == 3
        
        if (any(rlcen.observations)) {
          
            if (ncol(y) == 2) {
              
                ambiguous <- (y[, 1] != y[, 2]) & rlcen.observations
                
                if (any(ambiguous)) {
                  
                  cat("\n Left or right-censored observation with two unequal responses\n")
                  print(cbind(y[ambiguous, , drop = F], censor = the.censor.codes[ambiguous]))
                  fatal <- T
                }
            }
        }
    }
    if (!is.null(xmat(data.ld = x))) {
      
        if (nrow(xmat(data.ld = x)) != nrow(y)) {
          
            cat("\nNumber of rows in x matrix\n not equal to the number of rows in response:",
                nrow(xmat(data.ld = x)), nrow(y))
            fatal <- T
        }
    }
    invisible()
    if (fatal)
        stop("\nFatal data-specifcation error detected in SMRD.sanity.life.data;\ncheck the detailed message.")
    return(x)
}
