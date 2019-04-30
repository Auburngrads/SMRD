altplan <-
function (alt.plan.values, 
          plan.type, 
          use.condition, 
          highest.condition,
          censor.time, 
          quantile = 0.1, 
          xihold = 0.5, 
          pifix = 0.2, 
          pmlim = 0.2,
          kprint = 0, 
          method = NULL, 
          sample.size = 100)
{
    number.levels <- 3
    eta <- logb(censor.time)
    xu <- f.relationship(use.condition, alt.plan.values$relationship)
    xh <- f.relationship(highest.condition, alt.plan.values$relationship)
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
    
    idist <- numdist(alt.plan.values$distribution)
    idist.single <- floor((idist + 1)/2)
    relationship <- alt.plan.values$relationship
    maxstress <- 3
    if (kprint > 0) browser()
    
    zout <- APLAN(as.double(a), 
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
                  as.integer(kprint))
    
    allocation <- floor(zout$numvec$pip * sample.size)
    allocation[1] <- sample.size - sum(allocation[-1])
    
    the.plan <- get.alt.test.plan.direct(accel.variable.levels = f.relationshipinv(xu + zout$numvec$fpp * (xh - xu), relationship), 
                                         number.of.units = allocation,
                                         censor.times = rep(censor.time, number.levels), 
                                         accelvar.names = .InsertPeriods(alt.plan.values$accelvar.units),
                                         describe.string = describe.string)
    
    return(the.plan)
    
}
