get.rmodel.info <-
function (distribution, 
          model = 0, 
          explan.vars = NULL) 
{
    dist.pnames <- get.dist.pnames(distribution)
    
    if (model == 2) dist.pnames <- c(dist.pnames, "p(nfail)")
    if (model == 3) dist.pnames <- c(dist.pnames, "p(doa)")
    
    mrelat <- NULL
    model.pnames <- NULL
    
    if (is.vector(explan.vars) & !is.list(explan.vars)) {
      
        explan.vars <- list(mu.relat = explan.vars)
        
    }
    
    mu.relat    <- explan.vars$mu.relat
    sigma.relat <- explan.vars$sigma.relat
    prob.relat  <- explan.vars$prob.relat
    
    ncol.mrelat <- sum(c(!is.null(mu.relat), 
                         !is.null(sigma.relat), 
                         !is.null(prob.relat)))
    
    if (ncol.mrelat > 0) {
      
        nrow.mrelat <- max(length(mu.relat), 
                           length(sigma.relat), 
                           length(prob.relat))
        
        mrelat <- matrix(0, nrow = nrow.mrelat, ncol = ncol.mrelat)
        kparv <- NULL
        col.now <- 0
        
        if (!is.null(mu.relat)) {
          
            `if`(ncol.mrelat > 1,
                 name.qual <- paste(dist.pnames[1], "-", sep = ""),
                 name.qual <- "")
          
            model.pnames <- c(model.pnames, 
                              paste(paste("b", name.qual, sep = ""), 
                                    c(0, mu.relat), sep = ""))
            
            col.now <- col.now + 1
            kparv <- c(kparv, 1)
            mrelat[, col.now] <- c(mu.relat, 
                                   rep(0, nrow.mrelat - length(mu.relat)))
            
        } else {
          
            model.pnames <- c(model.pnames, dist.pnames[1])
            
        }
        
        if (!is.null(sigma.relat)) {
          
            if (is.element(generic.distribution(distribution), 
                           c("exponential", "logexponential"))) {
              
                stop("No sigma parameter for the exponential distribution")
              
            }
          
            name.qual <- paste(dist.pnames[2], "-", sep = "")
            
            model.pnames <- c(model.pnames, 
                              paste(paste("b", name.qual, sep = ""), 
                                    c(0, sigma.relat), sep = ""))
            
            col.now <- col.now + 1
            kparv <- c(kparv, 2)
            mrelat[, col.now] <- c(sigma.relat, 
                                   rep(0, nrow.mrelat - length(sigma.relat)))
            
        } else {
          
            model.pnames <- c(model.pnames, dist.pnames[2])
            
        }
        
        if (!is.null(prob.relat)) {
          
            if (!(model == 1 || model == 2)) stop("No probability in model")
          
            name.qual <- paste(dist.pnames[3], "-", sep = "")
            model.pnames <- c(model.pnames, 
                              paste(paste("b", name.qual, sep = ""), 
                                    c(0, prob.relat), sep = ""))
            
            col.now <- col.now + 1
            kparv <- c(kparv, 3)
            mrelat[, col.now] <- c(prob.relat, 
                                   rep(0, nrow.mrelat - length(prob.relat)))
            
        } else {
          
            if (model == 2 || model == 3) 
                model.pnames <- c(model.pnames, dist.pnames[3])
            
        }
        
        nrelat <- ncol(mrelat)
        
    } else {
      
        mrelat <- matrix(1, nrow = 1, ncol = 1)
        kparv <- 1
        nrelat <- 0
        model.pnames <- dist.pnames
        
    }
    
    nrvar <- apply(mrelat, 2, function(x) sum(x != 0))
    
    return.list <- list(explan.vars = explan.vars, 
                        kparv = kparv, 
                        mrelat = mrelat, 
                        nrelat = nrelat, 
                        nrvar = nrvar, 
                        dist.pnames = dist.pnames, 
                        model.pnames = model.pnames)
    
    return(return.list)
    
}
