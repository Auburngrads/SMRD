library(smrdfortran)
library(SMRD)
test = 1
if(test == 1) {
data.ld <- frame.to.ld(heatexchanger,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4)
}
if(test == 2) data.ld <- frame.to.ld(lzbearing, response.column = 1)
if(test == 3) {
  
   data.ld <- frame.to.ld(superalloy,
                          response.column = 1,
                          censor.column = 2,
                          case.weight.column = 3)
   
}
if(test == 4) {
  
data.ld <- frame.to.ld(smrdfortran::doatrun,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4,
                       truncation.response.column = 5,
                       truncation.type.column = 6)

}
if(test == 5){
  
  data.ld <- frame.to.ld(bearingcage,
                         response.column = 1, 
                         censor.column = 2, 
                         case.weight.column = 3,
                         time.units = "Hours")
  
}

number.sim = 2000
if(!exists("kprint")) kprint = 0
maxit = 500 
max.sim.scratch.space = 1000
maxmsd = 100
debug1 = F
randomize = !T 
  
    `if`(randomize,
         tspass <- runif(33),
         tspass <- seq(0.1, 0.4, length = 33))
    
    nty <- 0
    the.censor.codes <- smrdfortran:::censor.codes(data.ld)
    the.case.weights <- smrdfortran:::case.weights(data.ld)
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    ndscrat <- 3 * number.cases + 4
    niscrat <- 6 * number.cases + 7
    nrscrat <- max(7 * (number.cases + 1), (maxmsd * (maxmsd - 1))/2 + 1)
    sim.scratch.space <- min(sum(the.case.weights), 
                             max(max.sim.scratch.space, number.cases))
    cdfest.out <- cdfest(data.ld)
    m <- length(cdfest.out$p)
    number.things.returned <- 2 * m + 1
    if (debug1) browser()
    zout <- .Fortran("mlsim3", 
                     as.single(y), 
                     as.single(the.censor.codes), 
                     as.single(the.case.weights), 
                     as.integer(number.cases), 
                     as.integer(ny), 
                     as.integer(nty), 
                     ty = single(number.cases), 
                     tc = single(number.cases), 
                     gamthr = single(number.cases), 
                     as.integer(maxit), 
                     as.integer(kprint), 
                     double(ndscrat), 
                     integer(niscrat), 
                     single(nrscrat), 
                     as.single(cdfest.out$p), 
                     as.single(cdfest.out$q), 
                     single(m), 
                     single(m), 
                     as.integer(m), 
                     single(m), 
                     single(m), 
                     single(m), 
                     single(m), 
                     integer(sim.scratch.space), 
                     as.integer(sim.scratch.space),
                     wtnew = single(number.cases), 
                     ynew = single(number.cases * ny), 
                     return.matrix = single(number.sim * number.things.returned), 
                     as.integer(number.sim), 
                     as.integer(number.things.returned), 
                     as.single(tspass), 
                     as.logical(randomize), 
                     iersim = integer(1))

new = SMRD:::MLSIM3(y, 
                        the.censor.codes, 
                        the.case.weights,
                        number.cases, 
                        ny, 
                        nty, 
                        matrix(0, nrow = number.cases, ncol = 1), 
                        integer(number.cases), 
                     gamthr = double(number.cases), 
                     as.integer(maxit), 
                     as.integer(kprint), 
                     double(ndscrat), 
                     integer(niscrat), 
                     double(nrscrat), 
                     as.double(cdfest.out$p), 
                     as.double(cdfest.out$q), 
                     double(m + 1), 
                     double(m + 1), 
                     as.integer(m), 
                     double(m + 1), 
                     double(m + 1), 
                     double(m + 1), 
                     double(m + 1), 
                     integer(sim.scratch.space), 
                     as.integer(sim.scratch.space),
                     wtnew = double(number.cases), 
                     ynew  = matrix(3,nrow = number.cases, ncol = ny), 
                     retmat = matrix(4, ncol = number.sim, nrow =  number.things.returned), 
                     as.integer(number.sim), 
                     as.integer(number.things.returned), 
                     as.double(tspass), 
                     as.logical(randomize), 
                     iersim = integer(1))

old.return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
new.return.matrix <- t(new$nummat$retmat)
old.f.hat.star <- old.return.matrix[, 2:(m + 1)]
new.f.hat.star <- new.return.matrix[, 2:(m + 1)]
old.standard.errors <- old.return.matrix[, (m + 2):(2 * m + 1)]
new.standard.errors <- new.return.matrix[, (m + 2):(2 * m + 1)]
old.ierstuff <- as.integer(old.return.matrix[, 1])
new.ierstuff <- as.integer(new.return.matrix[, 1])
old.results <- list(cdfest.out = cdfest.out, 
                    p = cdfest.out$p, 
                    q = cdfest.out$q, 
                    f.hat = cdfest.out$prob, 
                    sd = cdfest.out$sd, 
                    f.hat.star = old.f.hat.star, 
                    stderror.star = old.standard.errors, 
                    ierstuff = old.ierstuff)
new.results <- list(cdfest.out = cdfest.out, 
                    p = cdfest.out$p, 
                    q = cdfest.out$q, 
                    f.hat = cdfest.out$prob, 
                    sd = cdfest.out$sd, 
                    f.hat.star = new.f.hat.star, 
                    stderror.star = new.standard.errors, 
                    ierstuff = new.ierstuff)

oldClass(new.results) <- "boot.npar.npar.out"

par(mfrow = c(1,2))
smrdfortran:::plot.boot.npar.npar.out(old.results)
smrdfortran:::plot.boot.npar.npar.out(new.results)
par(mfrow = c(1,1))
