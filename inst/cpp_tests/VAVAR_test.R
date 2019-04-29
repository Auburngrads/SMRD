library(smrdfortran)
mu = 0
sigma = 1
tc = 0.5
pe = 0.2
distribution = 'lev'

  lm <- length(mu)
  ls <- length(sigma)
  lt <- length(tc)
  lp <- length(pe)
  vlength <- max(lm, ls, lt, lp)
  mu    <- smrdfortran:::expand.vec(mu, vlength)
  sigma <- smrdfortran:::expand.vec(sigma, vlength)
  tc    <- smrdfortran:::expand.vec(tc, vlength)
  pe    <- smrdfortran:::expand.vec(pe, vlength)
  ze    <- smrdfortran:::quant(pe, distribution)
  zc    <- (logb(tc) - mu)/sigma
  switch(smrdfortran:::generic.distribution(distribution), sev = idist <- 1,
         lev = idist <- 2, normal = idist <- 3, logistic = idist <- 4,
                                                                  stop("Distribution must be sev, lev, normal, or logistic"))
  zout <- .Fortran("vavar", as.integer(idist), as.integer(vlength),
                   as.double(zc), as.double(ze), ans = double(vlength))

  zout
  
wqmmlesss::vavar(as.integer(idist),
            as.integer(vlength),
            as.double(zc),
            as.double(ze),
            double(vlength))
