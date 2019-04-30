testnum = 2

if(testnum == 1) {
library(smrdfortran)
lz.ld <- frame.to.ld(superalloy,
                     response.column = 1,
                     censor.column = 2,
                     case.weight.column = 3,
                     x.columns = c(4,5,6))
data.ld <- lz.ld
distribution = 'gamma'
theta.start = NULL
explan.vars = c(1,2,3)
mu.relat = NULL
sigma.relat = NULL
prob.relat = NULL
model = 0
}

if(testnum == 2) {
  
library(smrdfortran)
lz.ld <- frame.to.ld(superalloy,
                     response.column = 1,
                     censor.column = 2,
                     case.weight.column = 3,
                     x.columns = c(4,5,6))
data.ld <- lz.ld
distribution = "Weibull"
theta.start = NULL
explan.vars = list(mu.relat = c(2,3),
                   sigma.relat = c(2),
                   prob.relat = c(1))
model = 1
  
}
gamthr = 0
escale = 10000
e = rep(1e-04, nparm)
parameter.fixed = rep(F, nparm)
intercept = T
#model = 0
kprint = 0
conlev = 0.95
maxit = 500
debug1 = F

y <- Response(data.ld)
ncoly <- ncol(y)
number.cases <- nrow(y)
the.case.weights <- smrdfortran:::case.weights(data.ld)
ny <- ncol(y)
the.truncation.codes <- smrdfortran:::truncation.codes(data.ld)
if (is.null(the.truncation.codes)) {
  ty <- 1
  ncolty <- 0
  the.truncation.codes <- 1

} else {

  ty <- smrdfortran:::truncation.response(data.ld)
  ncolty <- ncol(ty)

}
distribution.number <- smrdfortran:::numdist(distribution)

  if (distribution.number == 14) distribution.number <- 8

  cat("dist num =", distribution, distribution.number, "\n")
    the.censor.codes <- smrdfortran:::censor.codes(data.ld)

    if (length(gamthr) == 1)
      gamthr <- rep(gamthr, number.cases)

      if (length(gamthr) != number.cases)
        stop("specified offset is the wrong length")

        get.rmodel.info.out <- smrdfortran:::get.rmodel.info(distribution,
                                               model,
                                               explan.vars)
        explan.vars <- get.rmodel.info.out$explan.vars

        if (get.rmodel.info.out$nrelat == 0) {

          regression <- F
          ncol.orig.x <- 0
          int <- 1
          the.xmat <- cbind(rep(1, number.cases))

        } else {

          the.xmat <- xmat(data.ld)
          ncol.orig.x <- ncol(the.xmat)
          if (is.null(the.xmat))
            stop("Explanatory variables requested, but there is no X matrix")
            regression <- T
          if (nrow(the.xmat) != number.cases)
            stop(paste("Number of rows in x matrix ", nrow(the.xmat),
                       " is wrong"))
            uniq.explan.vars <- unique(get.rmodel.info.out$mrelat)

         #   if (any(uniq.explan.vars<=0))
         #     stop("Negative or 0 explanatory variables column specified")

              if (max(uniq.explan.vars) > ncol(the.xmat))
                stop("Specified explanatory variable column greater than number of columns in X matrix")

                if (intercept) {

                  int <- 1
                  the.xmat <- cbind(rep(1, number.cases), the.xmat)

                } else {

                  int <- 0

                }

        }
old   <- .Fortran("gensiz",
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

new <- GENSIZ(as.integer(model),
                  as.integer(distribution.number),
                  as.integer(get.rmodel.info.out$kparv),
                  as.integer(get.rmodel.info.out$nrvar),
                  get.rmodel.info.out$mrelat,
                  as.integer(get.rmodel.info.out$nrelat),
                  as.integer(max(get.rmodel.info.out$nrvar)),
                  as.integer(ncol.orig.x),
                  as.integer(kprint),
                  nparm = integer(1),
                  npard = integer(1),
                  ier = integer(1),
                  nxd = as.integer(rep(0,5)),
                  intd = as.integer(rep(1000,5)),
                  ipxcd = vector(mode = "list", length = 5),
                  irelad = as.integer(rep(1,5)),
                  ilabp = as.integer(rep(0,80)),
                  ilabd = as.integer(rep(0,40)),
                  nregr = as.integer(0),
                  kgtall = as.integer(1),
                  llog = as.integer(0),
                  kmodp = as.integer(0),
                  npardm = as.integer(5),
                  nnum = as.integer(0),
                  kparm = as.integer(0),
                  iup = as.integer(0),
                  nterd = as.integer(0),
                  maxpd = as.integer(20))
