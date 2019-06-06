# start with loop.R
if(!exists("kprint")) kprint = 0
B = 1
numsim = B

test = 1
if(test == 1) {
    
p.fail = 0.2
m = 5
e.of.r = 4 #c(2, 5, 10, 20, 50, 100)
beta = 0.8 #c(0.8,1, 1.5, 3)
mu = 0
debug1 = F

results.list <- list()
kindex <- 0
i = 1
j = 1
        
          cat("\ne.of.r=", e.of.r[i], "beta=", beta[j], "\nCensor Scheme:\n")
            censor.scheme <- smrdfortran:::get.censor.scheme.all(m = m, 
                                                   p.fail = p.fail,
                                                   beta[j], e.of.r[i])
            
            assign(envir = .frame0,  inherits = TRUE,"tmp2.censor.scheme", censor.scheme)
            tmp.censor.scheme <- censor.scheme
            efail <- censor.scheme$number.units * pweibull(censor.scheme$censor.times, beta[j], scale = 1)
            #print(efail)
            #print(sum(efail))
            #print(censor.scheme)
            theta = c(mu, 1 / beta[j])
            
}
if(test == 2) {
    
    p.fail = 0.2
m = 5
e.of.r = c(2, 5, 10, 20, 50, 100)
beta = c(0.8,1, 1.5, 3)
mu = 0
debug1 = F

results.list <- list()
kindex <- 0
for (i in 1:length(e.of.r)) {
   
     for (j in 1:length(beta)) {
        
          cat("\ne.of.r=", e.of.r[i], "beta=", beta[j], "\nCensor Scheme:\n")
            censor.scheme <- smrdfortran:::get.censor.scheme.all(m = m, 
                                                   p.fail = p.fail,
                                                   beta[j], e.of.r[i])
            
            assign(envir = .frame0,  inherits = TRUE,"tmp2.censor.scheme", censor.scheme)
            tmp.censor.scheme <- censor.scheme
            efail <- censor.scheme$number.units * pweibull(censor.scheme$censor.times, beta[j], scale = 1)
            #print(efail)
            #print(sum(efail))
            #print(censor.scheme)
            theta = c(mu, 1 / beta[j])
            
}
}
}
   
# onto mlesim.mmr.stagger.R
censor.scheme = censor.scheme
distribution = "Weibull"
predict.time.delta = 1
explan.vars = NULL
escale = 10000
intercept = T
maxit = 500
debug1 = debug1
randomize = T

    censor.times <- censor.scheme$censor.times
    tr.censor.times <- ifelse(smrdfortran:::is.logdist(distribution), 
                              logb(censor.times),
                              censor.times)
    
    pfail.vec <- smrdfortran:::wqmf.phibf((tr.censor.times - theta[1])/theta[2],distribution)
    efail <- sum(censor.scheme$number.units * pfail.vec)
    sdfail <- sqrt(efail)
    number.intervals <- length(censor.scheme[[1]])
    max.number.cases <- max(efail + 7 * sdfail + 2 * number.intervals,
                            500 + 2 * number.intervals)
    nty <- 0
    ny <- 1
    nter <- 1
    int <- 1
    distribution.number <- smrdfortran:::numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    e = rep(1e-04, number.parameters)
    parameter.fixed = rep(F, number.parameters)

    if (smrdfortran:::generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        theta.start[number.parameters] <- 1
        parameter.fixed[number.parameters] <- T
    }
    ndscrat <- 4 * max.number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1
    niscrat <- 2 * (number.parameters + 1) + 2 * max.number.cases
    ngroup <- length(censor.scheme$number.units)
    krfail <- rep(0, ngroup)
    number.things.returned <- 5
    if (as.numeric(debug1) >= 1) { }
    if (as.numeric(debug1) >= 3) browser()

zout <- .Fortran("mlsim7", 
                 xmat = single(nter * max.number.cases),
                 y = single(ny * max.number.cases), 
                 cen = single(max.number.cases),
                 wt = single(max.number.cases), 
                 nrow = as.integer(max.number.cases),
                 nter = as.integer(nter), 
                 ny = as.integer(ny), 
                 nty = as.integer(nty),
                 ty = single(max.number.cases), 
                 truncation.codes = single(max.number.cases),
                 kdist = as.integer(distribution.number), 
                 gamthr = single(max.number.cases),
                 lfix = as.logical(parameter.fixed), 
                 krfail = as.integer(krfail),
                 nparm = as.integer(number.parameters), 
                 int = as.integer(int),
                 escale = as.single(escale), 
                 e = as.single(e), 
                 maxit = as.integer(maxit),
                 kprint = as.integer(kprint), 
                 dscrat = double(100 * ndscrat), 
                 iscrat = integer(100 * niscrat), 
                 devian = single(max.number.cases),
                 thetah = single(number.parameters), 
                 first.derivative = single(number.parameters),
                 vcv.matrix = single(number.parameters * number.parameters),
                 correlation.matrix = single(number.parameters * number.parameters),
                 residuals = single(ny * max.number.cases), 
                 fitted.values = single(ny * max.number.cases), 
                 theta = as.single(theta),
                 return.matrix = single(numsim * number.things.returned),
                 numsim = as.integer(numsim), 
                 prdelt = as.single(predict.time.delta),
                 ngroup = as.integer(ngroup), 
                 centim = as.single(censor.scheme$censor.times),
                 nsamsz = as.integer(censor.scheme$number.units),
                 nmrvec = integer(ngroup), 
                 nsimg = integer(1), 
                 numret = as.integer(number.things.returned),
                 nnomle = integer(1), 
                 iersim = integer(1))
        
new <- SMRD:::MLSIM7(x = matrix(0, ncol = nter, nrow = max.number.cases),
                         y = matrix(0, ncol = ny, nrow = max.number.cases), 
                         cen = integer(max.number.cases),
                         wt = integer(max.number.cases), 
                         nrow = as.integer(max.number.cases),
                         nter = as.integer(nter), 
                         ny = as.integer(ny), 
                         nty = as.integer(nty),
                         ty = matrix(0,nrow = max.number.cases, ncol = 1), 
                         tc = double(max.number.cases),
                         kdist = as.integer(distribution.number), 
                         gamthr = double(max.number.cases),
                         lfix = as.logical(parameter.fixed), 
                         krfail = as.integer(krfail),
                         nparm = as.integer(number.parameters), 
                         int = as.integer(int),
                         escale = as.double(escale), 
                         e = as.double(e), 
                         maxit = as.integer(maxit),
                         kprint = as.integer(kprint), 
                         dscrat = double(1), 
                         iscrat = integer(1), 
                         devian = matrix(0,nrow = max.number.cases, ncol = 3),
                         thetah = double(number.parameters), 
                         fsder = double(number.parameters),
                         vcv = matrix(0, nrow = number.parameters,ncol = number.parameters),
                         r = matrix(0, nrow = number.parameters,ncol = number.parameters),
                         res = matrix(0, nrow = max.number.cases,ncol = ny), 
                         fv = matrix(0, nrow = max.number.cases,ncol = ny), 
                         theta = as.double(theta),
                         retmat = matrix(0,ncol = numsim, nrow = number.things.returned),
                         numsim = as.integer(numsim), 
                         prdelt = as.double(predict.time.delta),
                         ngroup = as.integer(ngroup), 
                         centim = as.double(censor.scheme$censor.times),
                         nsamsz = as.integer(censor.scheme$number.units),
                         nmrvec = integer(ngroup), 
                         nsimg = integer(1), 
                         numret = as.integer(number.things.returned),
                         nnomle = integer(1), 
                         iersim = integer(1))



#            kindex <- kindex + 1
#            results.name <- paste("e.of.r=", e.of.r[i], ",beta=", beta[j], sep = "")
#            fraction.bad <- (attr(results, "numsim") - nrow(results))/attr(results, "numsim")
#            attr(results, "fraction.bad") <- fraction.bad
#            attr(results, "e.of.r") <- e.of.r
#            attr(results, "beta") <- beta
#            results.attributes <- attributes(results)
#            results <- as.single(results)
#            attributes(results) <- results.attributes
#            results.list[[results.name]] <- results
#        }
#    }
