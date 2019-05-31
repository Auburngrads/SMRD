## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny(cache = T)
library(SMRD2)

## ---- eval=FALSE---------------------------------------------------------
#  Fan.egeng.gmle.out <- FillRegion(Fan.egeng.gmle.out,
#                                   nbound = 10,
#                                   iter = 500)
#  
#  summary(Fan.egeng.gmle.out.jcr)
#  
#  names(Fan.egeng.gmle.out.jcr)
#  
#  basic.gmleprobplot(Fan.ld,distribution = "egeng",
#                     xlim = c(200,99999),
#                     ylim = c(.0011,.69),
#                     xxx.mle.out = Fan.egeng.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = .95,
#                     ciMethod = "lr.approx",
#                     length.time.vec = 2)
#  
#  bear.egeng.gmle.out <- egeng.mle(lzbearing.ld)
#  
#  
#  Fan.weibull.gmle.out <- ls2.mle(Fan.ld, distribution = "weibull")
#  
#  
#  Fan.weibull.gmle.out <-  FillRegion(Fan.weibull.gmle.out,
#                                      nbound = 4,
#                                      iter = 50,
#                                      cull = 2 )
#  
#  summary(Fan.weibull.gmle.out.jcr)
#  summary(Fan.weibull.gmle.out)
#  
#  mleprobplot(Fan.ld,
#              distribution = "Weibull",
#              xlim = c(200,9999),
#              ylim = c(.0031,.49))
#  
#  basic.gmleprobplot(Fan.ld,
#                     distribution = "Weibull",
#                     xlim = c(200,9999),
#                     ylim = c(.0031,.49),
#                     xxx.mle.out = Fan.weibull.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = .95,
#                     length.time.vec = 30)
#  
#  basic.gmleprobplot(Fan.ld,
#                     distribution = "Weibull",
#                     xlim = c(200,9999),
#                     ylim = c(.0031,.49),
#                     xxx.mle.out = Fan.weibull.gmle.out,
#                     my.title = "",
#                     cexlab = 1.5,
#                     conlev = 0.95,
#                     ciMethod = "lr.approx",
#                     length.time.vec = 30)

## ---- eval=FALSE---------------------------------------------------------
#  fcn = function(theta,time,distribution){
#  
#        f.phibf((log(time)-theta[1]) / exp(theta[2]),
#                        distribution = "Weibull")
#  
#  }
#  
#  Fr.conf(fcn,
#          fcn.arg2 = log.seq(200,2000,length=10),
#          gmle.out = Fan.weibull.gmle.out,
#          ptwise = T)
#  
#  Fr.conf(fcn,
#          fcn.arg2 = log.seq(200,2000,length = 10),
#          gmle.out = Fan.weibull.gmle.out,
#          ptwise = T,
#          extrapolate = T)

