library(smrdfortran)
library(SMRD)

test = 1

if(test == 1){
#from asd.quant
plan.values3 <- smrdfortran::get.plan.values("Weibull",
                                prob = c(.2,.12),
                                time = c(1000,500), 
                                time.units = "Hours")
plan.values = plan.values3 
n = 50
censor.time = 1000
quantile.mark = 0.1

    idist <- numdist(smrdfortran:::basic.distribution(plan.values$distribution))
    
    if (any(quantile.mark <= 0) || any(quantile.mark >= 1)) {
        
        stop("Specified quanttile not between 0-1.")
        
    }
    zc <- (logb(censor.time) - plan.values$mu) / plan.values$sigma
    ze <- smrdfortran:::quant(quantile.mark, plan.values$distribution)
    vlength <- max(length(zc), length(ze))
    zc <- smrdfortran:::expand.vec(zc, vlength)
    ze <- smrdfortran:::expand.vec(ze, vlength)
}

if(test == 2){

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
  
}
  zout <- .Fortran("vavar", as.integer(idist), as.integer(vlength),
                   as.double(zc), as.double(ze), ans = double(vlength))

  new <- SMRD:::VAVAR(as.integer(idist),
            as.integer(vlength),
            as.double(zc),
            as.double(ze),
            double(vlength))
