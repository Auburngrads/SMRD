two.stage.dest.degrad <-
function (trans.data.ddd, 
          distribution, 
          group.var = 1, 
          stresses = get.x.markers(trans.data.ddd, group.var = group.var), 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          debug1 = map.SMRDDebugLevel(), 
          double.count.zeros = T)
{
    do.list <- function (data.d) { return(attr(data.d, "do.list")) }
    the.do.list <- do.list(trans.data.ddd)
    
    if (is.null(the.do.list))
        stop("do.list missing in two.stage.dest.degrad")
    
    the.complete.list <- complete.list(the.do.list)
    slope <- rep(NA, length(the.do.list))
    sample.size <- rep(NA, length(the.do.list))
    the.done.list <- rep("", length(the.do.list))
    the.slope.computed.list <- rep("", length(the.do.list))
    intercept <- rep(NA, length(the.do.list))
    x.values <- matrix(NA, nrow = length(the.do.list), ncol = ncol(xmat(trans.data.ddd)))
    sigma <- rep(NA, length(the.do.list))
    se.slope <- rep(NA, length(the.do.list))
    k <- 0
    sigma2 <- 0
    for (i in 1:length(the.do.list)) {
      
        all.times <- as.matrix(times(trans.data.ddd))[, 1]
        the.ones.restrict.zero <- the.complete.list == the.do.list[i]
        the.ones <- the.complete.list == the.do.list[i] | (double.count.zeros &
            all.times == 0)
        xmat.time <- as.matrix(times(trans.data.ddd)[the.ones])
        unique.times <- unique(xmat.time)
        if (length(unique.times) > 1) {
            k <- k + 1
            the.done.list[k] <- the.do.list[i]
            xmat.time <- as.matrix(times(trans.data.ddd)[the.ones])
            dimnames(xmat.time) <- list(NULL, "Time")
            tmp.data <- 
              make.frame.ld(y = Response(trans.data.ddd)[the.ones],
                            the.censor.codes = censor.codes(trans.data.ddd)[the.ones],
                            the.case.weights = case.weights(trans.data.ddd)[the.ones],
                            the.xmat = xmat.time, 
                            time.units = get.time.units(trans.data.ddd))
            
            mlest.out <- mlest(tmp.data, 
                               distribution = distribution,
                               explan.vars = 1,
                               embedded = T)
            
            se.slope[k] <- sqrt(mlest.out$vcv.matrix[2, 2])
            if (debug1 > 3)
                cat("two.stage", i, the.do.list[i], "\n")
            x.values[k, ] <- unique(as.matrix(xmat(trans.data.ddd))[the.ones.restrict.zero,
                ])
            slope[k] <- mlest.out$theta.hat[2]
            sigma[k] <- mlest.out$theta.hat[3]
            intercept[k] <- mlest.out$theta.hat[1]
            sample.size[k] <- length(the.ones[the.ones])
            sigma2 <- sigma2 + sigma[k]^2 * sample.size[k]
        }
    }
    dimnames(x.values) <- list(as.character(1:nrow(x.values)),
                               dimnames(xmat(trans.data.ddd))[[2]])
    length(slope) <- k
    length(se.slope) <- k
    length(the.done.list) <- k
    x.values <- x.values[1:k, , drop = F]
    length(intercept) <- k
    length(sigma) <- k
    length(sample.size) <- k
    slope.lower <- rep(NA, k)
    slope.upper <- rep(NA, k)
    
    `if`(all(slope > 0) || all(slope < 0),
         the.kodet <- 2,
         the.kodet <- 1)
    
    the.kodet <- 2
    for (i in 1:k) {
        the.ci <- compute.confidence.interval(slope[i], se.slope[i],
            kodet = the.kodet, conf.level = conf.level)
        slope.lower[i] <- the.ci$fun.lower
        slope.upper[i] <- the.ci$fun.upper
    }
    slopes.gt.0 <- slope > 0
    frac.gt.0 <- length(slope[slopes.gt.0])/length(slope)
    average.slope <- median(slope)
    
    `if`(frac.gt.0 > 0.5,
         increasing <- T,
         increasing <- F)
    
    if (increasing) {
        ok.values <- slope > 0
        sign.factor <- 1
  } else {
        ok.values <- slope < 0
        sign.factor <- -1
    }
    ok.slope <- slope[ok.values]
    the.slope.computed.list <- the.done.list
    the.done.list <- the.done.list[ok.values]
    ok.x.values <- x.values[ok.values, , drop = F]
    if (debug1> 4) {
      
        cat("ok.x.values   \n")
        print(ok.x.values)
        cat("ok.slope   \n")
        print(ok.slope)
    }
    
    if (nrow(ok.x.values) > ncol(ok.x.values)) {
      
        fit.out <- lm(logb(sign.factor * ok.slope) ~ ok.x.values)
        the.coeficients <- summary(fit.out)$coefficients
        dimnames(the.coeficients) <- list(NULL, NULL)
        sigma.pooled <- sqrt(sigma2/(sum(sample.size) - k))
        return.vector <- c(mean(intercept[ok.values]), 
                           sign.factor * exp(the.coeficients[, 1][1]), 
                           the.coeficients[, 1][-1],
                           sigma = sigma.pooled)
        
        beta2.names <- paste("beta", 
                             seq(2, ncol(ok.x.values) + 1), 
                             sep = "")
        
        the.names <- c("beta0", "beta1", beta2.names, "sigma")
        names(return.vector) <- the.names
        
  } else {
    
        warning(paste("Two-stage starting value algorithm failed! Bad model/data specification?",
            "\nSlopes=", paste(format(slope), collapse = ",")))
        return.vector <- NULL
        
  }
    
    attr(return.vector, "slope.computed.list") <- the.slope.computed.list
    attr(return.vector, "done.list") <- the.done.list
    attr(return.vector, "increasing") <- increasing
    attr(return.vector, "ok.values") <- ok.values
    attr(return.vector, "intercept") <- intercept
    attr(return.vector, "stress") <- x.values
    attr(return.vector, "slope.lower") <- slope.lower
    attr(return.vector, "slope.upper") <- slope.upper
    attr(return.vector, "slope") <- slope
    attr(return.vector, "sigma") <- sigma
    attr(return.vector, "se.slope") <- se.slope
    attr(return.vector, "sample.size") <- sample.size
    return(return.vector)
}
