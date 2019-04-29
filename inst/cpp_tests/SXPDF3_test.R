library(smrdfortran)
ndist1 = 2
ndist2 = 2
beta0 = 30.27241
beta1 = -5.100121
stress = 270
sigma = 0.2894549
ugamma = 5.365834
sgamma = 0.03140004
w = logb(5000)
debug1= F
kprint = 2
    
      max.length <- max(length(beta0), length(beta1), length(sigma),
                        length(ugamma), length(sgamma), length(stress), length(w))
      beta0  <- smrdfortran:::expand.vec(beta0, max.length)
      beta1  <- smrdfortran:::expand.vec(beta1, max.length)
      sigma  <- smrdfortran:::expand.vec(sigma, max.length)
      ugamma <- smrdfortran:::expand.vec(ugamma, max.length)
      sgamma <- smrdfortran:::expand.vec(sgamma, max.length)
      stress <- smrdfortran:::expand.vec(stress, max.length)
      w      <- smrdfortran:::expand.vec(w, max.length)
      if (debug1) browser()
      zout <- .Fortran("sxpdf3", as.integer(ndist1), as.integer(ndist2),
                       as.double(beta0), as.double(beta1), as.double(stress),
                       as.double(sigma), as.double(ugamma), as.double(sgamma),
                       as.double(w), as.integer(max.length), answer = double(max.length),
                       ier = integer(max.length))
      
      new = wqmmlesss::sxpdf3(as.integer(ndist1), as.integer(ndist2),
                       as.double(beta0), as.double(beta1), as.double(stress),
                       as.double(sigma), as.double(ugamma), as.double(sgamma),
                       as.double(w), as.integer(max.length), answer = double(max.length),
                       ier = integer(max.length),as.integer(kprint))
