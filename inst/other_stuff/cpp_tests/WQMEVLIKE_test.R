library(smrdfortran)
test = 5
#Start with get.big.posterior
if(test == 1) {
data.ld <- frame.to.ld(superalloy,
                       response.column = 1,
                       censor.column = 2,
                       case.weight.column = 3,
                       x.columns = 4)
explan.vars = 1
}

if(test == 2) {
data.ld <- frame.to.ld(bearingcage,
                       response.column = 1, 
                       censor.column = 2,
                       case.weight.column = 3)
explan.vars = NULL
}
if(test == 3) {
data.ld <- frame.to.ld(heatexchanger,
                       response.column = c(1,2), 
                       censor.column = 3,
                       case.weight.column = 4)
explan.vars = NULL
}
if(test == 4) {
data.ld <- frame.to.ld(superalloy,
                       response.column = 1, 
                       censor.column = 2,
                       case.weight.column = 3,
                       x.columns = c(4,5,6))
explan.vars = c(1,2)
}
if(test == 5){
  
   data.ld <- frame.to.ld(prob3_5, 
                          response.column = 1,
                          censor.column = 2)
   explan.vars = NULL
   }



specifications.for.prior <-
  specify.simple.prior( p  = .15,
                        qdist = "loguniform",
                        qlower = 100,
                        qupper = 5000,
                        sigma.dist = "weibull",
                        sigma.lower = 0.2,
                        sigma.upper = 5,
                        distribution  = "logistic")

number.needed = 2
number.in.init.prior = number.needed

 if (is.null(specifications.for.prior)) stop("prior spec must be given to make a posterior")
  
    number.in.old.post <- 0
    distribution <- attr(specifications.for.prior, "distribution")
    initial.prior <- smrdfortran:::make.prior(specifications.for.prior, 
                                number.in.init.prior)

# Moving to compute.posterior

prior = initial.prior
prior.temp <- prior
orig.data.ld <- data.ld
prior.temp$prior[, 1] <- logb(prior.temp$prior[, 1])
## added new 
x.columns <- attr(data.ld, "x.columns")
prior.temp$relationships <- rep('linear', length(x.columns))
prior.temp$xs <- data.ld[,x.columns]

if (!is.null(explan.vars)) {
  
    relationships <- 
      smrdfortran:::set.relationship.power(prior.temp$relationships, power)
    x.columns <- attr(data.ld, "x.columns")
    
    for (i in explan.vars) {
      
        the.relationship <- smrdfortran:::subscript.relationship(relationships, i)
        
        data.ld[, x.columns[i]] <- 
          smrdfortran:::f.relationship(data.ld[, x.columns[i]], the.relationship)
        
        prior.temp$xs[i] <- 
          smrdfortran:::f.relationship(prior.temp$xs[i], the.relationship)
        
    }
    
} else { relationships <- NULL }

mlest.out <- mlest(data.ld, distribution, explan.vars = explan.vars)
xs.plus <- c(prior.temp$xs[explan.vars], 
             smrdfortran:::quant(prior.temp$p, distribution))

`if`(!is.null(explan.vars),
     col.use <- c((explan.vars + 1), ncol(prior.temp$prior)),
     col.use <- c(ncol(prior.temp$prior)))

prior.temp$prior[, 1] <- 
  prior.temp$prior[, 1] - as.matrix(prior.temp$prior[, col.use]) %*% xs.plus

mode(prior.temp$prior) <- "single"

## Now, move into like.eval - finally!

theta = prior.temp$prior[, c(1, col.use)]
gamthr = 0
intercept = T
kprint = 0
debug1 = F 
likelihood.method = GetsmrdfortranDefault("smrdfortran.likelihood.method")

    y <- smrdfortran:::Response(data.ld)
    ntheta <- nrow(theta)
    number.cases <- nrow(y)
    ny <- ncol(y)
    nty <- 0
    distribution.number <- smrdfortran:::numdist(distribution)
    the.case.weights    <- smrdfortran:::case.weights(data.ld)
    the.censor.codes    <- smrdfortran:::censor.codes(data.ld)
    
    if (length(gamthr) == 1) gamthr <- rep(gamthr, number.cases)
    
    if (length(gamthr) != number.cases) stop("specified offset is the wrong length")
    
    the.truncation.codes <- smrdfortran:::truncation.codes(data.ld)
    ty <- smrdfortran:::truncation.response(data.ld)
    
    if (!is.null(the.truncation.codes) && !is.null(ty)) {
      
        nty <- ncol(ty)
        
        } else {
          
        nty <- 0
        the.truncation.codes <- rep(1, length(the.censor.codes))
        ty <- rep(0, length(the.censor.codes))
        
        }
    
    if (is.null(explan.vars)) {
      
        regression <- F
        int <- 1
        the.xmat <- cbind(rep(1, number.cases))
        param.names <- c("mu", "sigma")
      
    } else {
          
        the.xmat <- smrdfortran:::xmat(data.ld)
        if (is.null(the.xmat)) stop("Explanatory variables requested, but there is no X matrix")
        regression <- T
        if (nrow(the.xmat) != number.cases) {
            stop(paste("Number of rows in x matrix ", 
                       nrow(the.xmat), 
                       " is wrong"))
        }
        
        if (any(explan.vars <= 0)) stop("Negative or 0 explanatory variables column specified")
        if (max(explan.vars) > ncol(the.xmat)) stop("Specified explanatory variable column greater than number of columns in X matrix")
        
        if (intercept) {
          
            int <- 1
            the.xmat <- cbind(rep(1, number.cases), 
                              the.xmat[, explan.vars, drop = F])
            param.names <- c("b0", 
                             paste("b", explan.vars, sep = ""),
                             "sigma")
            
            } else {
              
            int <- 0
            the.xmat <- the.xmat[, explan.vars]
            param.names <- c(paste("b", 
                                   explan.vars, sep = ""), 
                             "sigma")
            }
        }
    y <- as.matrix(y - gamthr)
    mathsoft.gamthr <- rep(0, nrow(y))
    nter <- ncol(the.xmat)
    nparm <- nter + 1
    parameter.fixed = rep(F, nparm)
    dummy <- the.censor.codes
    
    if (!is.matrix(theta)) stop("Input theta must be a matrix")
    if (ncol(theta) != nparm) stop("Wrong number of theta columns")
    
    old <- .Fortran("SMRD", 
                     as.single(as.matrix(the.xmat)), 
                     as.single(y),
                     as.single(the.censor.codes), 
                     as.single(the.case.weights),
                     as.single(ty), 
                     as.single(the.truncation.codes),
                     as.single(mathsoft.gamthr),
                     nrow = as.integer(number.cases), 
                     as.integer(ny), 
                     as.integer(nty),
                     nparm = as.integer(nparm), 
                     as.integer(int), 
                     as.integer(nter),
                     theta = as.double(t(theta)), 
                     as.logical(parameter.fixed),
                     as.integer(ntheta), 
                     fpfxxx = double(1),
                     upcen2 = double(1),
                     kdist = as.integer(distribution.number),
                     thetb = double(nparm), 
                     thetg = double(nparm), 
                     xnew = double(ncol(the.xmat) * nrow(the.xmat)), 
                     diag = double(nparm), 
                     tmat = double(nparm^2),
                     rv1 = double(nparm), 
                     vcvg = double(nparm^2), 
                     as.integer(kprint),
                     xlike = double(ntheta), 
                     ierr = integer(1))
  
    new <- SMRD::WQMEVLIKE( 
                     as.matrix(the.xmat), 
                     y,
                     as.integer(the.censor.codes), 
                     as.integer(the.case.weights),
                     as.matrix(ty), 
                     as.integer(the.truncation.codes),
                     mathsoft.gamthr,
                     nrow = as.integer(number.cases), 
                     as.integer(ny), 
                     as.integer(nty),
                     nparm = as.integer(nparm), 
                     as.integer(int), 
                     as.integer(nter),
                     theta = t(theta), 
                     as.logical(parameter.fixed),
                     as.integer(ntheta), 
                     fpfxxx = double(1),
                     upcen = double(1),
                     kdist = as.integer(distribution.number),
                     thetb = double(nparm), 
                     thetg = double(nparm), 
                     xnew = matrix(0,ncol = ncol(the.xmat),nrow = nrow(the.xmat)), 
                     diag = double(nparm), 
                     tmat = matrix(0,nparm,nparm),
                     rv1 = double(nparm), 
                     vcvg = matrix(0,nparm,nparm), 
                     as.integer(kprint),
                     xlike = double(ntheta), 
                     ier = integer(1))

cat("diag"); any(abs(old$diag - new$diag) > 1e-6)
cat("rv1");  any(abs(old$rv1  - new$rv1) > 1e-6)
cat("thetag"); any(abs(old$thetg - new$thetg) > 1e-6)
cat("thetab"); any(abs(old$thetb - new$thetb) > 1e-6)
cat("vcvg"); any(abs(old$vcvg - new$vcvg) > 1e-6)
cat("xlike"); any(abs(old$xlike - new$xlike) > 1e-6)
cat("tmat"); any(abs(old$tmat - new$tmat) > 1e-6)
cat("xnew"); stu = vector(mode = 'numeric', length = length(old$xnew))
stu = for(i in 1:length(old$xnew)) stu[i] = abs(old$xnew[i] - new$xnew[i])
any(stu > 1e-6)
