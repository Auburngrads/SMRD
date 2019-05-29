## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
plan.values1 <- get.plan.values("Weibull", 
                                beta = 2, 
                                prob = .1, 
                                time = 100, 
                                time.units = "Hours")

## ------------------------------------------------------------------------
summary(plan.values1)

plot(plan.values1)
failure.probabilities(plan.values1)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 120,
#                       number.detail = 5,
#                       quantile.mark = 0.2,
#                       number.sim = 200)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 300,
#                       number.detail = 5,
#                       number.sim = 200)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values1,
#                       n = 50,
#                       censor.time = 1000,
#                       number.sim = 50,
#                       quantile.mark = 0.2)

## ------------------------------------------------------------------------
plan.values2 <- get.plan.values("Lognormal", 
                                sigma = 0.5,
                                prob = 0.1, 
                                time = 100, 
                                time.units = "Hours")

summary(plan.values2)
plot(plan.values2)

plot(plan.values2, 
     censor.time = 1000, 
     grids = F)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values2,
#                       n = 50,
#                       censor.time = 1000,
#                       quantile.mark = 0.1)

## ------------------------------------------------------------------------
plan.values3 <- get.plan.values("Weibull",
                                prob = c(.2,.12),
                                time = c(1000,500), 
                                time.units = "Hours")

plan.values4 <- get.plan.values("Weibull",
                                prob = c(.05,.15),
                                time = c(40000,100000),
                                time.units = "Hours")

summary(plan.values3)
plot(plan.values3)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(plan.values3,
#                       n = 50,
#                       censor.time = 1000,
#                       quantile.mark = 0.1)

## ------------------------------------------------------------------------
#compare the simulated value with the large-sample approx below

asd.quant(plan.values3, 
          n = 50, 
          censor.time = 1000, 
          quantile.mark = 0.1)

#compare:

asd.quant(plan.values3,
          n = 50, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(50)

asd.quant(plan.values3,
          n = 500, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(500)

asd.quant(plan.values3,
          n = 5000, 
          censor.time = 1000, 
          quantile.mark = 0.1) * sqrt(5000)

## ------------------------------------------------------------------------
# For the normal distribution
variance.factor(distribution = 'normal', 
                type = 'quantile', 
                quantile.of.interest = 0.02,
                proportion.failing = 0.2)


# For the smallest extreme value distribution
variance.factor(distribution = 'sev',
                type = 'quantile', 
                quantile.of.interest = 0.02,
                proportion.failing = 0.2)

## ------------------------------------------------------------------------
asym.test.plan.properties(plan.values3, 
                          n = 50, 
                          proportion.failing = 0.1)

asd.quant(plan.values3,
          n = 50, 
          censor.time = 1000, 
          quantile.mark = c(0.1, 0.3, 0.5, 0.63))

## ------------------------------------------------------------------------
lsinf(seq(-1,1, by = 0.1),"right","sev")

lsinf(seq(-2,2, by = 0.2),"right","normal")

## ------------------------------------------------------------------------
table.lines(seq(-1,1,by=.1),"sev")

table.lines(seq(-1,1,by=.1),"normal")

## ------------------------------------------------------------------------
variance.factor("sev", type = 'quantile')
variance.factor("normal", type = 'quantile')
variance.factor("logistic", type = 'quantile')


variance.factor("sev", type = 'hazard')
variance.factor("normal", type = 'hazard')
variance.factor("logistic", type = 'hazard')

## ------------------------------------------------------------------------
zero.failure.plan(xlim = c(1.51,3.99), 
                  ylim = c(.1,29), 
                  krange = c(1.5,3.83))

zero.failure.plan(betavec = c( 1., 2.), 
                  quantile = 0.01, 
                  conlev = 0.95, 
                  xlim = c(1.51,10), 
                  ylim = c(.1,199), 
                  krange = c(1.5,10),
                  grid = T,
                  bw = FALSE)

## ------------------------------------------------------------------------
zero.failure.k(beta = 2, quantile = 0.1, conlev = 0.99,	n = 5)

zero.failure.k(beta = 1, quantile = 0.01, conlev = 0.95, n = 5)

zero.failure.k(beta = 2, quantile = 0.01, conlev = 0.95, n = 5)

## ------------------------------------------------------------------------
zero.failure.n(conlev = 0.95, quantile = 0.01, k = 14, beta = 1)

zero.failure.n(conlev = 0.95, quantile = 0.01, k = 3.369, beta = 2)

zero.failure.prsd(alpha.vec = c(0.05,0.1), quantile = 0.01, pfactor = 3)

## ------------------------------------------------------------------------
bulb.plan.values1 <- get.plan.values("normal", 
                                     sigma = 85, 
                                     prob = 0.5,
                                     time = 1000,
                                     time.units = "Hours")

summary(bulb.plan.values1)
plot(bulb.plan.values1)

## ---- eval=FALSE---------------------------------------------------------
#  life.test.simulation(bulb.plan.values1,
#                       n = 50,
#                       censor.time = 1000,
#                       number.detail = 5,
#                       quantile.mark = 0.5)

## ------------------------------------------------------------------------
plot(plan.values3,
     censor.time = 100,
     quantile.of.interest = 0.1)

#here is an example using type 2 censoring

plot(plan.values3,
     fraction.failing = 0.1,
     quantile.of.interest = 0.1)

# In actual application, use number.sim = 10000 to get smoother curves

asym.sample.size(plan.values3,
                 censor.time = 500,
                 Rvalue = 1.5,
                 quantile.of.interest = 0.1)

asym.sample.size(plan.values3,
                 fraction.failing = 0.1,
                 Rvalue = 1.5,
                 quantile.of.interest = 0.1)

asym.sample.size(bulb.plan.values1,
                 fraction.failing = 0.1,
                 HalfWidth = 50,
                 quantile.of.interest = 0.1)

## ------------------------------------------------------------------------
SMRD2:::plot.prob.cs.type2("lognormal", 
                            k = 2,
                            n = c(5,10,20),
                            r = c(3,6,12), 
                            number.sim = 100)

SMRD2:::plot.prob.cs.type2("loglogistic", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12), 
                           number.sim = 100)

SMRD2:::plot.prob.cs.type2("weibull", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12), 
                           number.sim = 100)

SMRD2:::plot.prob.cs.type2("frechet", 
                           k = 2,
                           n = c(5,10,20),
                           r = c(3,6,12),
                           number.sim = 100)

