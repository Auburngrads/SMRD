rflm.loglike <-
function (thetatran, kprint = 0)
{
  xcdf <-
    function (ndist1 = 2, 
              ndist2 = 2,
              beta0 = 30.27241, 
              beta1 = -5.100121,
              stress = 270, 
              sigma = 0.2894549, 
              ugamma = 5.365834, 
              sgamma = 0.03140004,
              w = logb(5000),
              debug1 = F)
    {
      max.length <- max(length(beta0), 
                        length(beta1),
                        length(sigma),
                        length(ugamma),
                        length(sgamma),
                        length(stress),
                        length(w))
      
      beta0 <- expand.vec(beta0, max.length)
      beta1 <- expand.vec(beta1, max.length)
      sigma <- expand.vec(sigma, max.length)
      ugamma <- expand.vec(ugamma, max.length)
      sgamma <- expand.vec(sgamma, max.length)
      stress <- expand.vec(stress, max.length)
      w <- expand.vec(w, max.length)
      if (debug1) browser()
      
      zout <- SXCDF(as.integer(ndist1), 
                    as.integer(ndist2),
                    as.double(beta0), 
                    as.double(beta1), 
                    as.double(stress),
                    as.double(sigma), 
                    as.double(ugamma),
                    as.double(sgamma),
                    as.double(w), 
                    as.integer(max.length), 
                    answer = double(max.length),
                    ier = integer(max.length),
                    as.integer(kprint))
      
      return(zout$answer)
      
    }

  xpdf <-
    function (ndist1, 
              ndist2,
              beta0, 
              beta1, 
              stress, 
              sigma, 
              ugamma,
              sgamma, 
              w,
              debug1 = F)
    {
      max.length <- max(length(beta0), 
                        length(beta1),
                        length(sigma),
                        length(ugamma),
                        length(sgamma),
                        length(stress),
                        length(w))
      
      beta0 <- expand.vec(beta0, max.length)
      beta1 <- expand.vec(beta1, max.length)
      sigma <- expand.vec(sigma, max.length)
      ugamma <- expand.vec(ugamma, max.length)
      sgamma <- expand.vec(sgamma, max.length)
      stress <- expand.vec(stress, max.length)
      w <- expand.vec(w, max.length)
      
      if (debug1) browser()
      
      zout <- SXPDF3(as.integer(ndist1), 
                     as.integer(ndist2),
                     as.double(beta0), 
                     as.double(beta1), 
                     as.double(stress),
                     as.double(sigma), 
                     as.double(ugamma), 
                     as.double(sgamma),
                     as.double(w),
                     as.integer(max.length), 
                     answer = double(max.length),
                     ier = integer(max.length),
                     as.integer(kprint))
      
      return(zout$answer)
      
    }

    data.ld <- get(envir = .frame0, "data.ld")
    debug1 <- get(envir = .frame0, "debug1")
    model <- get(envir = .frame0, "model")
    f.origparam <- model$f.origparam
    iter.count <- get(envir = .frame0, "iter.count") + 1
    assign(envir = .frame0,  inherits = TRUE, "iter.count", iter.count)
    cond.dist <- model$cond.dist
    fl.dist <- model$fl.dist
    theta <- f.origparam(thetatran, model)
    beta0 <- theta[1]
    beta1 <- theta[2]
    sigma <- theta[3]
    mu.gamma <- theta[4]
    sigma.gamma <- theta[5]
    x <- xmat(data.ld)[, 1]
    y <- logb(Response(data.ld))
    ccodes <- censor.codes(data.ld)
    censored <- ccodes == 2
    failed <- ccodes == 1
    runout <- 0
    if ((iter.count < 4 && debug1 > 1)) browser()
    
    if (any(censored)) {
      
        y.cens <- y[censored]
        x.cens <- x[censored]
        cdf.val <- xcdf(cond.dist, 
                        fl.dist, 
                        beta0, 
                        beta1, 
                        x.cens,
                        sigma, 
                        mu.gamma, 
                        sigma.gamma, 
                        y.cens)
        
        `if`(any(is.na(cdf.val)) | any(cdf.val >= 1),
             return(1e+10),
             runout <- sum(logb(1 - cdf.val)))
        
    }
    
    pdf.val <- xpdf(cond.dist, 
                    fl.dist, 
                    beta0, 
                    beta1, 
                    x[failed],
                    sigma, 
                    mu.gamma, 
                    sigma.gamma, 
                    y[failed])
    
    pdf.val[pdf.val <= 0] <- 1e-10
    
    `if`(any(pdf.val <= 0) | any(is.na(pdf.val)),
         return(1e+10),
         failure <- sum(logb(pdf.val)))
    
    log.like <- runout + failure
    if ((iter.count < 4 && debug1> 1 || debug1 > 2) || debug1 > 4 || iter.count < 0) {
        print(paste("in rflm.loglike Iter=", iter.count, paste(c("log.like",
            "runout", "failure"), collapse = " "), "=", paste(format(c(log.like,
            runout, failure)), collapse = " ")))
        print(paste("in rflm.loglike", paste(model$orig.param.names,
            collapse = " "), "=", paste(format(c(beta0, beta1,
            sigma, mu.gamma, sigma.gamma)), collapse = " ")))
    }
    
    return(Uminus(log.like))
    
}
