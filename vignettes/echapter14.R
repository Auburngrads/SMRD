## ---- echo = FALSE-------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
BearingCage.ld <- frame.to.ld(bearingcage,
                              response.column = 1, 
                              censor.column = 2,
                              case.weight.column = 3)
DT::datatable(BearingCage.ld)

## ------------------------------------------------------------------------
bcage.prior.weibull.spec1 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 100,
                       qupper = 5000,
                       sigma.dist =  "lognormal",
                       sigma.lower = 0.2,
                       sigma.upper =  0.5,
                       distribution = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.weibull.spec2 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.5,
                       sigma.upper = 2.5, 
                       distribution  = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.weibull.spec3 <-
  specify.simple.prior(p = .01,
                       qdist = "lognormal",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.5,
                       sigma.upper = 2.5,
                       distribution  = "Weibull")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec1 <-
  specify.simple.prior( p  = .04,
                        qdist = "loguniform",
                        qlower = 100,
                        qupper = 5000,
                        sigma.dist = "lognormal",
                        sigma.lower = 0.2,
                        sigma.upper = 5,
                        distribution  = "Lognormal")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec2 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1,
                       sigma.upper =  1.5,
                       distribution = "Lognormal")

## ------------------------------------------------------------------------
bcage.prior.lognormal.spec3 <-
  specify.simple.prior(p = .01,
                       qdist = "loguniform",
                       qlower = 1000,
                       qupper = 1400,
                       sigma.dist = "lognormal",
                       sigma.lower = 1.,
                       sigma.upper = 1.5,
                       distribution  = "Lognormal")

## ------------------------------------------------------------------------
prior2.bcage <- 
  make.prior(spec = bcage.prior.lognormal.spec1, 
             number.in.prior = 3000)


prior.and.post2.bcage <-
  get.big.posterior(bcage.prior.lognormal.spec1,
                    BearingCage.ld)

prior.and.post2.bcage$post[1:10,]

prior.and.post3.bcage <- 
  make.small.posterior.object(prior.and.post2.bcage)

## ------------------------------------------------------------------------
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = T,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

#quantle marginal
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#sigma marginal
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = T,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#prob
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Marginals only",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = T)

#Joint only axes.range.default.post = T 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             axes.range.default.post = T,
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = T)

#Joint only 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

#Joint only 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1,
                             include.likelihood = T)

#Joint only axes.range.default.post = F 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             axes.range.default.post = F,
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = F)

#Joint only axes.range.default.post = F 
summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             axes.range.default.post = F, 
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000,
                             include.likelihood = F)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu")

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 6000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint only",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Parameter",
                             newdata = "mu",
                             include.likelihood = T)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = T,
                             type.position = "Parameter",
                             newdata = "mu")

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Quantile",
                             newdata = .1)

summarize.posterior.or.prior(prior.and.post2.bcage,post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "prior",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 1000)

summarize.posterior.or.prior(prior.and.post2.bcage,
                             post.or.prior = "post",
                             task = "Joint and Marginal",
                             marginal.on.sigma = F,
                             marginal.on.pos = F,
                             type.position = "Failure probability",
                             newdata = 6000)


prior.and.post3.bcage <- make.small.posterior.object(prior.and.post2.bcage)

SMRD2:::plot.prediction(prior.and.post2.bcage, 
                time.range = log(c(500,20000000)),
                xlab = "Hours")

## ------------------------------------------------------------------------
SMRD2:::plot.prediction.order(x = 1,
                      nsamsize = 3,
                      prior.and.post2.bcage,
                      time.range = log(c(50,200000)),
                      xlab = "Hours")

## ------------------------------------------------------------------------
SMRD2:::plot.prediction.order(x = 1,
                      nsamsize = 50,
                      prior.and.post2.bcage,
                      time.range = log(c(10,100000)),
                      xlab = "Hours")

