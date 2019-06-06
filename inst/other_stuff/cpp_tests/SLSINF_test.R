library(smrdfortran)
z <- -3
censor.type = 'right'
distribution = 'logistic'
if (!is.numeric(z))
  stop("z must be numeric")
  if (!is.character(censor.type))
    stop("censor.type must be character")
    if (!is.character(distribution))
      stop("distribution must be character string")
      switch(smrdfortran:::generic.distribution(distribution), weibull = idist <- 2, sev = idist <- 1,
             frechet = , lev = idist <- 2, lognormal = , normal = idist <- 3,
                                                                         loglogistic = , logistic = idist <- 4, stop("Distribution must be sev, lev, normal, or logistic"))
      switch(censor.type, uncensored = icode <- 1, right = icode <- 2,
             left = icode <- 3, stop("censor.type must be uncensored, left, or right"))
      nrows <- length(z)
      #if (debug1) browser()
        zout <- .Fortran("slsinf", as.integer(idist), as.integer(icode),
                         as.double(z), as.double(z), f11 = double(nrows), f12 = double(nrows),
                         f22 = double(nrows), as.integer(nrows), ifault = integer(1),
                         irow = integer(1))

new = SMRD::slsinf(as.integer(idist),
       as.integer(icode),
       as.double(z),
       as.double(z),
       f11 = double(nrows),
       f12 = double(nrows),
       f22 = double(nrows),
       as.integer(nrows),
       ifault = integer(1),
       irow = integer(1))

zout[5:7]
new
