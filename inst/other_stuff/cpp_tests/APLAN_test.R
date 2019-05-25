library(SMRD)
library(SMRD2)
alt.plan.values <- SMRD:::get.alt.plan.values.from.slope.and.point(
  distribution="Weibull",
  relationship="Arrhenius",
  accelvar.units=c("DegreesC"),
  time.units="Days", 
  censor.time=183,
  probs=c(.001), 
  accelvar=c(50), 
  slope=0.726, 
  beta = 1.667)
plan.type = "Optimum"
use.condition = 50
highest.condition = 120
censor.time = 183
quantile = 0.1
sample.size = 300
xihold = 0.5 
pifix = 0.2 
pmlim = 0.2
kprint = 0
method = NULL

  number.levels <- 3
  eta <- logb(censor.time)
  xu <- SMRD:::f.relationship(use.condition, alt.plan.values$relationship)
  xh <- SMRD:::f.relationship(highest.condition, alt.plan.values$relationship)
  a <- (eta - (alt.plan.values$theta.vec["beta0"] + alt.plan.values$theta.vec["beta"] *
    xu))/alt.plan.values$sigma
  b1 <- (alt.plan.values$theta.vec["beta"] * (xh - xu))/alt.plan.values$sigma
  b2 <- 0
  theta <- 1
  known <- 2
  switch(plan.type, Optimum = {
  pifix <- 0
    iopta <- 3
    iopts <- 2
    ioptm <- 0
    describe.string <- "Optimum"
  }, `Optimized compromise` = {
    iopta <- 3
    iopts <- 2
    ioptm <- 0
    describe.string <- "Optimized compromize"
  }, `Equal expected` = {
    iopta <- 2
    iopts <- 2
    ioptm <- 0
    describe.string <- "Equal expected number failing"
  }, `421` = {
    iopta <- 6
    iopts <- 2
    ioptm <- 0
    describe.string <- "421"
  }, Traditional = {
    iopta <- 1
    iopts <- 2
    ioptm <- 0
    describe.string <- "Traditional"
  }, {
    stop(paste("plan type not recognized", plan.type))
  })
  
  idist <- SMRD:::numdist(alt.plan.values$distribution)
  idist.single <- floor((idist + 1)/2)
  relationship <- alt.plan.values$relationship
  maxstress <- 3
  
zout <- .Fortran("aplan", a = as.double(a), b1 = as.double(b1),
                 b2 = as.double(b2), theta = as.double(theta), quantile = as.double(quantile),
                 known = as.integer(known), idist.single = as.integer(idist.single),
                 iopts = as.integer(iopts), iopta = as.integer(iopta),
                 ioptm = as.integer(ioptm), pifix = as.double(pifix),
                 xihold = as.double(xihold), pmlim = as.double(pmlim),
                 xi = double(maxstress), pi = double(maxstress), fp = double(maxstress),
                 pq = double(maxstress), var = double(1), kprint)

new = SMRD2:::APLAN(as.double(a), 
                  as.double(b1),
                  as.double(b2), 
                  as.double(theta), 
                  as.double(quantile),
                  as.integer(known), 
                  as.integer(idist.single),
                  as.integer(iopts), 
                  as.integer(iopta),
                  as.integer(ioptm), 
                  as.double(pifix),
                  as.double(xihold), 
                  as.double(pmlim),
                  double(maxstress), 
                  double(maxstress), 
                  double(maxstress),
                  double(maxstress), 
                  double(1), 
                  iprinp = as.integer(kprint),
                  ier = integer(1))
