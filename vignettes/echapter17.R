## ---- echo=FALSE---------------------------------------------------------
SMRD2:::vinny()
library(SMRD2)

## ------------------------------------------------------------------------
comptime.ld <- frame.to.ld(comptime,
                           response.column = 3, 
                           x.column = 2,
                           time.units = "Seconds", 
                           xlabel = "System.Load")

summary(comptime.ld)

censored.data.plot(comptime.ld,
                   xlab = "System Load")

## ---- eval=FALSE---------------------------------------------------------
#  comptime.mlest.out <- groupm.mleprobplot(comptime.ld,
#                                           distribution ="Lognormal",
#                                           group.var = 1,
#                                           relationship = "linear")
#  
#  comptime.mlest.out2 <- groupm.mleprobplot(comptime.ld,
#                                            distribution ="Normal",
#                                            group.var = 1,
#                                            relationship = "linear")
#  
#  SMRD2:::resid.vs.order(comptime.mlest.out)
#  
#  SMRD2:::resid.vs.fit(comptime.mlest.out)
#  
#  SMRD2:::resid.vs.explan(comptime.mlest.out)
#  
#  SMRD2:::resid.probplot(comptime.mlest.out)
#  
#  plot(comptime.mlest.out,
#       density.at = c(1, 3, 5))
#  
#  plot(comptime.mlest.out,
#       density.at = c(1, 3, 5),
#       response.on.yaxis = F)
#  
#  #or more simply as
#  
#  plot(comptime.mlest.out)
#  
#  quantiles(comptime.mlest.out,new.data=5)

