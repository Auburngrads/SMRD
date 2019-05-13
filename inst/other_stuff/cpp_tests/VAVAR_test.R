library(SMRD)
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
  mu    <- SMRD:::expand.vec(mu, vlength)
  sigma <- SMRD:::expand.vec(sigma, vlength)
  tc    <- SMRD:::expand.vec(tc, vlength)
  pe    <- SMRD:::expand.vec(pe, vlength)
  ze    <- SMRD:::quant(pe, distribution)
  zc    <- (logb(tc) - mu)/sigma
  switch(SMRD:::generic.distribution(distribution), sev = idist <- 1,
         lev = idist <- 2, normal = idist <- 3, logistic = idist <- 4,
                                                                  stop("Distribution must be sev, lev, normal, or logistic"))
  zout <- .Fortran("vavar", as.integer(idist), as.integer(vlength),
                   as.double(zc), as.double(ze), ans = double(vlength))

  zout <- SMRD2::VAVAR(as.integer(idist),
            as.integer(vlength),
            as.double(zc),
            as.double(ze),
            double(vlength))
