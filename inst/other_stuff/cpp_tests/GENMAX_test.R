library(smrdfortran)
library(SMRD)
test = 1
if(test == 1) {
data.ld <- frame.to.ld(superalloy,
                     response.column = 1,
                     censor.column = 2,
                     case.weight.column = 3,
                     x.columns = c(4,5,6))
distribution = "weibull"
theta.start = NULL
explan.vars = list(mu.relat = c(2,3),
                   sigma.relat = c(2))
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}

if(test == 2) {
data.ld <- frame.to.ld(superalloy,
                     response.column = 1,
                     censor.column = 2,
                     case.weight.column = 3,
                     x.columns = c(4,5,6))
distribution = "weibull"
theta.start = NULL
explan.vars = list(mu.relat = c(2,3))
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}
if(test == 3) {
data.ld <- frame.to.ld(superalloy,
                     response.column = 1,
                     censor.column = 2,
                     case.weight.column = 3,
                     x.columns = c(4,5,6))
distribution = "weibull"
theta.start = NULL
explan.vars = NULL
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}
if(test == 4) {
data.ld <- frame.to.ld(lzbearing,
                     response.column = 1)
distribution = "lognormal"
theta.start = NULL
explan.vars = NULL
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}
if(test == 5) {
data.ld <- frame.to.ld(heatexchanger,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4)
distribution = "generalized gamma"
theta.start = NULL
explan.vars = NULL
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}
if(test == 6){
  ConnectionStrength.ld <- 
  frame.to.ld(connectionstrength,
              response.column = 1,
              failure.mode.column = 2,
              case.weight.column = 3)


mfm.to.ld(ConnectionStrength.ld)

  data.ld <- ConnectionStrength.Bond.ld
  
  
distribution = "normal"
theta.start = NULL
explan.vars = NULL
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0

}
gamthr = 0
escale = 10000
intercept = T
if(!exists("kprint")) kprint <- 0
conlev = 0.95
maxit = 500
debug1 = F

  y <-Response(data.ld)
  ncoly <- ncol(y)
  number.cases <- nrow(y)
  the.case.weights <- SMRD:::case.weights(data.ld)
  ny <- ncol(y)
  the.truncation.codes <- SMRD:::truncation.codes(data.ld)
  if (is.null(the.truncation.codes)) {
    ty <- rep(1,number.cases)
    ncolty <- 0
    the.truncation.codes <- rep(1,number.cases)

  } else {

    ty <- SMRD:::truncation.response(data.ld)
    ncolty <- ncol(ty)

  }
  distribution.number <- SMRD:::numdist(distribution)

    if (distribution.number == 14) distribution.number <- 8

    cat("dist num =", distribution, distribution.number, "\n")
      the.censor.codes <- SMRD:::censor.codes(data.ld)

      if (length(gamthr) == 1)
        gamthr <- rep(gamthr, number.cases)

        if (length(gamthr) != number.cases)
          stop("specified offset is the wrong length")

          get.rmodel.info.out <- SMRD:::get.rmodel.info(distribution,
                                                 model,
                                                 explan.vars)
          explan.vars <- get.rmodel.info.out$explan.vars

          if (get.rmodel.info.out$nrelat == 0) {

            regression <- F
            ncol.orig.x <- 0
            int <- 1
            the.xmat <- cbind(rep(1, number.cases))

          } else {

            the.xmat <- SMRD:::xmat(data.ld)
            ncol.orig.x <- ncol(the.xmat)
            if (is.null(the.xmat))
              stop("Explanatory variables requested, but there is no X matrix")
              regression <- T
            if (nrow(the.xmat) != number.cases)
              stop(paste("Number of rows in x matrix ", nrow(the.xmat),
                         " is wrong"))
              uniq.explan.vars <- unique(get.rmodel.info.out$mrelat)

              #if (any(uniq.explan.vars<=0))
              #  stop("Negative or 0 explanatory variables column specified")

                if (max(uniq.explan.vars) > ncol(the.xmat))
                  stop("Specified explanatory variable column greater than number of columns in X matrix")

                  if (intercept) {

                    int <- 1
                    the.xmat <- cbind(rep(1, number.cases), the.xmat)

                  } else {

                    int <- 0

                  }

          }
          zsize <- .Fortran("gensiz",
                            as.integer(model),
                            as.integer(distribution.number),
                            as.integer(get.rmodel.info.out$kparv),
                            as.integer(get.rmodel.info.out$nrvar),
                            as.integer(get.rmodel.info.out$mrelat),
                            as.integer(get.rmodel.info.out$nrelat),
                            as.integer(max(get.rmodel.info.out$nrvar)),
                            as.integer(ncol.orig.x),
                            as.integer(kprint),
                            nparm = integer(1),
                            npard = integer(1),
                            ier = integer(1),
                            nxd = integer(5),
                            intd = integer(5),
                            ipxcd = integer(5),
                            irelad = integer(5),
                            index = integer(1),
                            nterd = integer(1),
                            itry = integer(1),
                      ipb = integer(1),
                      ips = integer(1),
                      ipe = integer(1),
                      iis = integer(200))

            nparm <- zsize$nparm
            npard <- zsize$npard

            if (is.null(theta.start)) {

              lstart <- 0
              theta.start <- double(nparm)

            } else {

              lstart <- 1

            }

            e = rep(1e-04, nparm)
parameter.fixed = rep(F, nparm)
            
            if (any(parameter.fixed) == T) {

              `if`(is.null(theta.start),
                   stop("Fixed parameters requested, but theta.start empty"),
                   theta <- theta.start)

            } else {

              theta <- double(nparm)

            }
            kparv <- get.rmodel.info.out$kparv
              nrvar <- get.rmodel.info.out$nrvar
              mrelat <- get.rmodel.info.out$mrelat
              nrelat <- max(get.rmodel.info.out$nrelat, 1)
              mnrvar <- max(get.rmodel.info.out$nrvar)

              
 if (debug1) browser()

if(F) {
  
    old <- .Fortran("genmax", 
                     as.integer(model), 
                     as.integer(distribution.number),
                     as.double(rep(1,nparm)), 
                     double(nparm),
                     kodet = integer(nparm), 
                     as.integer(parameter.fixed),
                     as.integer(nparm), 
                     as.integer(npard), 
                     as.single(as.matrix(y)), 
                     as.integer(ncoly),
                     as.integer(number.cases), 
                     as.single(as.matrix(the.xmat)),
                     as.integer(ncol.orig.x), 
                     as.single(the.censor.codes),
                     as.single(the.case.weights), 
                     as.single(ty), 
                     as.integer(ncolty),
                     as.single(the.truncation.codes), 
                     as.integer(kprint),
                     as.integer(kparv), 
                     as.integer(nrvar), 
                     as.integer(mrelat),
                     as.integer(nrelat), 
                     as.integer(mnrvar), 
                     xlogl = double(1),
                     yhat = single(length(y)), 
                     resid = single(length(y)),
                     vcvs = double(nparm * nparm), 
                     vcv = double(nparm * nparm), 
                     r = double(nparm * nparm),
                     as.double(theta.start), 
                     as.integer(lstart), 
                     as.double(conlev),
                     ilabp = integer(8 * nparm), 
                     ilabd = integer(8 * nparm),
                     ier = integer(1),
                     nxd = as.integer(rep(0,5)),
                     intd = as.integer(rep(1000,5)),
                     ipxcd = as.integer(rep(0,5)),
                     irelad = as.integer(rep(1,5)),
                     fstder = double(12),
                     nregr = as.integer(0), 
                     kcentr = as.integer(1), 
                     kpoint = as.integer(0), 
                     ifit   = as.integer(2),
                     kgtall = as.integer(1), 
                     llog   = as.integer(0), 
                     kmodp  = as.integer(0),
                     maxit  = as.integer(50),
                     pest = double(1), 
                     epsx = as.double(1.0e-10),
                     npardm = as.integer(5),
                     nnum = as.integer(0),
                     kparm = as.integer(0),
                     iup = as.integer(0),
                     nterd = as.integer(0),
                     maxpd = as.integer(20),
                     pfail = as.double(0),
                     kmccde = as.integer(0),
                     nstart = as.integer(0),
                     maxmsd = as.integer(1),
                     tol    = as.double(1.0e-2),
                     lsd = as.integer(1),
                     pchmax = as.double(0))
}

new <- SMRD:::GENMAX(as.integer(model), 
             as.integer(distribution.number),
             as.double(rep(0,nparm)), 
             double(nparm),
             kodet = integer(nparm), 
             as.integer(parameter.fixed),
             as.integer(nparm), 
             as.integer(npard), 
             as.matrix(y), 
             as.integer(ncoly),
             as.integer(number.cases), 
             as.matrix(the.xmat),
             as.integer(ncol.orig.x), 
             as.integer(the.censor.codes),
             as.integer(the.case.weights), 
             as.matrix(ty), 
             as.integer(ncolty),
             as.integer(the.truncation.codes), 
             as.integer(kprint),
             as.integer(kparv), 
             as.integer(nrvar), 
             as.matrix(mrelat),
             as.integer(nrelat), 
             as.integer(mnrvar), 
             xlogl = double(1),
             yhat = matrix(0,dim(y)), 
             resid = matrix(0,dim(y)),
             vcvs = matrix(0,nparm, nparm), 
             vcv = matrix(0,nparm, nparm), 
             r = matrix(0,nparm, nparm),
             as.double(theta.start), 
             as.integer(lstart), 
             as.double(conlev),
             ilabp = integer(8 * nparm), 
             ilabd = integer(8 * nparm),
             ier = integer(1),
             nxd = as.integer(rep(0,5)),
             intd = as.integer(rep(1000,5)),
             ipxcd = list(0,0,0,0,0),
             irelad = as.integer(rep(1,5)),
             fstder = double(nparm),
             nregr = as.integer(0), 
             kcentr = as.integer(1), 
             kpoint = as.integer(0), 
             ifit   = as.integer(2),
             kgtall = as.integer(1), 
             llog   = as.integer(0), 
             kmodp  = as.integer(0),
             maxit  = as.integer(50),
             pest = as.double(0), 
             epsx = as.double(1.0e-10),
             npardm = as.integer(5),
             nnum = as.integer(0),
             kparm = as.integer(0),
             iup = as.integer(0),
             nterd = as.integer(0),
             maxpd = as.integer(20),
             pfail = as.double(0),
             kmccde = as.integer(0),
             nstart = as.integer(0),
             maxmsd = as.integer(0),
             tol    = as.double(1.0e-2),
             lsd = as.integer(1),
             pchmax = as.double(0))
