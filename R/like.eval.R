like.eval <-
function (data.ld, 
          distribution, 
          theta, 
          explan.vars = NULL, 
          gamthr = 0,
          parameter.fixed = rep(F, nparm), 
          intercept = T, 
          kprint = 0,
          debug1 = F, 
          likelihood.method = GetSMRDDefault("SMRD.likelihood.method"))
{
    y <- Response(data.ld)
    ntheta <- nrow(theta)
    number.cases <- nrow(y)
    ny <- ncol(y)
    nty <- 0
    distribution.number <- numdist(distribution)
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    
    if (length(gamthr) == 1)
        gamthr <- rep(gamthr, number.cases)
    
    if (length(gamthr) != number.cases)
        stop("specified offset is the wrong length")
    
    the.truncation.codes <- truncation.codes(data.ld)
    ty <- truncation.response(data.ld)
    
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
          
        the.xmat <- xmat(data.ld)
        if (is.null(the.xmat))
            stop("Explanatory variables requested, but there is no X matrix")
        regression <- T
        if (nrow(the.xmat) != number.cases)
            stop(paste("Number of rows in x matrix ", 
                       nrow(the.xmat), 
                       " is wrong"))
        
        if (any(explan.vars) <= 0)
            stop("Negative or 0 explanatory variables column specified")
        if (max(explan.vars) > ncol(the.xmat))
            stop("Specified explanatory variable column greater than number of columns in X matrix")
        
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
    dummy <- the.censor.codes
    
    if (!is.matrix(theta))
        stop("Input theta must be a matrix")
    if (ncol(theta) != nparm)
        stop("Wrong number of theta columns")
    
    zout <- WQMEVLIKE(as.matrix(the.xmat), 
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
    
    return(zout$xlike)
}
