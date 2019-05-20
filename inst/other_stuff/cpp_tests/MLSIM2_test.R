library(SMRD)
library(SMRD2)
test = 4
if(test == 1){
  
   data.ld <- frame.to.ld(lzbearing, response.column = 1) 
   
}
if(test == 2){
  
   data.ld <- frame.to.ld(heatexchanger, 
                          response.column = c(1,2),
                          censor.column = 3,
                          case.weight.column = 4) 
}
if(test == 3){
  
   data.ld <- frame.to.ld(prob3_5, 
                          response.column = 1,
                          censor.column = 2)
}
if(test == 4){
  
  data.ld <- frame.to.ld(bearingcage,
                         response.column = 1, 
                         censor.column = 2, 
                         case.weight.column = 3,
                         time.units = "Hours")
  
}
 
distribution = "weibull"
number.sim = 50
escale = 10000
intercept = T
if(!exists("kprint")) kprint = 0
maxit = 500
max.sim.scratch.space = 1000
debug1 = F
randomize = T

    
    `if`(randomize,
         tspass <- runif(33),
         tspass <- seq(0.1, 0.4, length = 33))
    
    the.censor.codes <- SMRD2:::censor.codes(data.ld)
    the.case.weights <- SMRD2:::case.weights(data.ld)
    nty <- 0
    nter <- 1
    int <- 1
    mlest.out <- mlest(data.ld, 
                       distribution = distribution, 
                       kprint = kprint)
    
    theta.start <- mlest.out$theta.hat
    theta.hat <- theta.start
    distribution.number <- SMRD2:::numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    
    if (SMRD2:::generic.distribution(distribution) == "exponential") {
      
       distribution.number <- 2
       parameter.fixed[number.parameters] <- T
       number.parametersx <- 1
       
    }
    iret <- 3
    number.things.returned <- number.parameters + ((number.parameters) * 
                                                     (number.parameters + 1))/2 + 2
    y <- SMRD2:::Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    the.xmat <- matrix(1, nrow = number.cases, ncol = 1)
    ndscrat <- number.parameters * 
               number.cases + 5 * 
               number.parameters *
               number.parameters + 12 * 
               number.parameters + 1
    niscrat <- 2 * (number.parameters + 1)
    sim.scratch.space <- min(sum(the.case.weights), 
                             max(max.sim.scratch.space, number.cases))
    e = rep(1e-04, number.parameters)
    parameter.fixed = rep(F, number.parameters)

    
    if (debug1) browser()
    
    zout <- .Fortran("mlsim2", 
                     as.single(the.xmat), 
                     as.single(y), 
                     as.single(the.censor.codes), 
                     as.single(the.case.weights), 
                     as.integer(number.cases), 
                     as.integer(nter), 
                     as.integer(ny), 
                     as.integer(nty), 
                     ty = single(number.cases), 
                     tc = single(number.cases), 
                     distribution.number = as.integer(distribution.number), 
                     gamthr = single(number.cases), 
                     parameter.fixed = as.logical(parameter.fixed), 
                     number.parameters = as.integer(number.parameters), int = as.integer(int), 
                     escale = as.single(escale), 
                     e = as.single(e), 
                     maxit = as.integer(maxit), 
                     kprint = as.integer(kprint), 
                     dscrat = double(ndscrat), 
                     iscrat = integer(niscrat), 
                     devian = single(number.cases * 3), 
                     thetah = as.single(theta.start), 
                     first.derivative = single(number.parameters), 
                     vcv.matrix = single(number.parameters * number.parameters), 
                     correlation.matrix = single(number.parameters * number.parameters), 
                     residuals = single(ny * number.cases), 
                     fitted.values = single(ny * number.cases), 
                     theta = single(number.parameters), 
                     iarray = integer(sim.scratch.space), 
                     marray = as.integer(sim.scratch.space), 
                     wtnew = single(number.cases), 
                     xnew = single(number.cases * nter), 
                     ynew = single(number.cases * ny), 
                     iret = as.integer(iret), 
                     return.matrix = single(number.sim * number.things.returned), 
                     number.sim = as.integer(number.sim), 
                     numret = as.integer(number.things.returned), 
                     tspass = as.single(tspass), 
                     lrand = as.logical(randomize), 
                     iersim = integer(1))
    
new = SMRD2:::MLSIM2(x = as.matrix(the.xmat), 
                     y = as.matrix(y), 
                     cen = as.integer(the.censor.codes), 
                     wt = as.integer(the.case.weights), 
                     nrow = as.integer(number.cases), 
                     nter = as.integer(nter), 
                     ny = as.integer(ny), 
                     nty = as.integer(nty), 
                     ty = matrix(0,nrow = number.cases, ncol = 1), 
                     tcodes = integer(number.cases), 
                     kdist = as.integer(distribution.number), 
                     gamthr = double(number.cases), 
                     lfix = as.logical(parameter.fixed), 
                     nparm = as.integer(number.parameters), 
                     intcpt = as.integer(int), 
                     escale = as.double(escale), 
                     e = as.double(e), 
                     maxit = as.integer(maxit), 
                     kprint = as.integer(kprint), 
                     dscrat = double(ndscrat), 
                     iscrat = integer(niscrat), 
                     devian = matrix(0, nrow = number.cases, ncol = 3), 
                     thetah = as.double(theta.start), 
                     fsder = double(number.parameters), 
                     vcv = matrix(0, nrow = number.parameters, ncol = number.parameters), 
                     r = matrix(0, nrow = number.parameters, ncol = number.parameters), 
                     res = matrix(0, ncol = ny, nrow = number.cases), 
                     fv = matrix(0, ncol = ny, nrow = number.cases), 
                     theta = double(number.parameters), 
                     iarray = integer(sim.scratch.space), 
                     marray = as.integer(sim.scratch.space), 
                     wtnew = integer(number.cases), 
                     xnew = matrix(0, nrow = number.cases, ncol = nter), 
                     ynew = matrix(0, nrow = number.cases, ncol = ny), 
                     iret = as.integer(iret), 
                     retmat = matrix(4, ncol = number.sim, nrow = number.things.returned), 
                     numsim = as.integer(number.sim), 
                     numret = as.integer(number.things.returned), 
                     tspass = as.double(tspass), 
                     lrand = as.logical(randomize), 
                     iersim = integer(1))

old.return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
new.return.matrix <- t(new$nummat$retmat)
old.theta.hat.star <- old.return.matrix[, 2:(number.parameters + 1)]
new.theta.hat.star <- new.return.matrix[, 2:(number.parameters + 1)]
old.ierstuff <- as.integer(old.return.matrix[, 1])
new.ierstuff <- as.integer(new.return.matrix[, 1])
old.vcv <- old.return.matrix[, (number.parameters + 3):(number.parameters + ((number.parameters) * (number.parameters + 1))/2 + 2)]
new.vcv <- new.return.matrix[, (number.parameters + 3):(number.parameters + ((number.parameters) * (number.parameters + 1))/2 + 2)]

    if (SMRD:::generic.distribution(distribution) == "exponential") {
        old.theta.hat.star[, 1] <- exp(old.theta.hat.star[, 1])
        new.theta.hat.star[, 1] <- exp(new.theta.hat.star[, 1])
        theta.hat[1] <- exp(theta.hat[1])
        mlest.out$theta.hat <- theta.hat
        mlest.out$vcv.matrix[1, 1] <- mlest.out$vcv.matrix[1,1] * theta.hat[1] ^ 2
        old.vcv[, 1] <- old.vcv[, 1] * ((old.theta.hat.star[, 1]) ^ 2)
        new.vcv[, 1] <- new.vcv[, 1] * ((new.theta.hat.star[, 1]) ^ 2)
        names(mlest.out$theta.hat)[1] <- "Theta"
    }

old.results <- list(theta.hat = theta.hat, 
                    theta.hat.star = old.theta.hat.star,
                    ierstuff = old.ierstuff, 
                    likelihood = old.return.matrix[, number.parameters + 2], 
                    vcv = old.vcv, 
                    mlest.out = mlest.out)
new.results <- list(theta.hat = theta.hat, 
                    theta.hat.star = new.theta.hat.star,
                    ierstuff = new.ierstuff, 
                    likelihood = new.return.matrix[, number.parameters + 2], 
                    vcv = new.vcv, 
                    mlest.out = mlest.out)

oldClass(old.results) <- "boot.npar.par.out"
oldClass(new.results) <- "boot.npar.par.out"

par(mfrow = c(1,2))
SMRD2:::plot.boot.npar.par.out(old.results)
SMRD2:::plot.boot.npar.par.out(new.results)
par(mfrow = c(1,1))

