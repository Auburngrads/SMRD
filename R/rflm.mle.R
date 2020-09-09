#' Title
#'
#' @param data.ld 
#' @param cond.dist 
#' @param fl.dist 
#' @param theta.start 
#' @param tran.theta.start 
#' @param debug1 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' LaminatePanel.ld <- frame.to.ld(laminatepanel,
#'                                 response.column = "kilocycles",
#'                                 time.units = "kilocycles",
#'                                 data.title = "Laminate Panel Fatigue Data",
#'                                 censor.column = "event", 
#'                                 x.column = "mpa")
#' 
#' censored.data.plot(LaminatePanel.ld, 
#'                    x.axis = "log", 
#'                    y.axis = "log",
#'                    response.on.yaxis = F)
#' 
#' censored.data.plot(LaminatePanel.ld)
#' 
#' LaminatePanel.fit11.out <- rflm.mle(LaminatePanel.ld,
#'                                     cond.dist = "sev", 
#'                                     fl.dist = "sev")
#' 
#' plot(LaminatePanel.fit11.out,response.on.yaxis = F)
#' 
#' LaminatePanel.fit11f.out <- rflm.mle(LaminatePanel.ld,
#'                                      cond.dist = "sev", 
#'                                      fl.dist = "sev",
#'                                      debug = 1,
#'                                      fixed.param.list =   
#'                                      list(fixed.parameters = 3,
#'                                           fixed.parameter.values = 0.20))
#' 
#' plot(LaminatePanel.fit11f.out,
#'      response.on.yaxis = F)
#' 
#' 
#' }
rflm.mle <-
function (data.ld, 
          cond.dist, 
          fl.dist, 
          theta.start = NULL, 
          tran.theta.start = T, 
          debug1 = F, ...) 
{
    options(digits = 9)
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    
    rfl.dist.map <- function(distname) {
        
        gdistname <- generic.distribution(distname)
        switch(gdistname, sev = {
            return(1)
        }, normal = {
            return(2)
        }, logistic = {
            return(3)
        }, stop("Distribution name not recognized"))
        
    }
    
    xmat(data.ld) <- as.matrix(xmat(data.ld))
    the.xvec <- xmat(data.ld)[, 1]
    ccodes <- censor.codes(data.ld)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    
    f.tranparam <- function(thetaorig, model) {
        
        beta0.x <- thetaorig[1] + thetaorig[2] * model$mean.lx
        beta1 <- thetaorig[2]
        log.sigma <- logb(thetaorig[3])
        log.sigma.gamma <- logb(thetaorig[5])
        mu.gamma.x <- (thetaorig[4] - model$mean.low3rd) / thetaorig[5]
        thetatran <- c(beta0.x, 
                       beta1, 
                       log.sigma,
                       mu.gamma.x, 
                       log.sigma.gamma)
        
        names(thetatran) <- model$t.param.names
        return(thetatran)
        
    }
    
    f.origparam <- function(thetatran, model) {
        beta0 <- thetatran[1] - thetatran[2] * model$mean.lx
        beta1 <- thetatran[2]
        sigma <- exp(thetatran[3])
        sigma.gamma <- exp(thetatran[5])
        mu.gamma <- model$mean.low3rd + sigma.gamma * thetatran[4]
        thetaorig <- c(beta0, beta1, sigma, mu.gamma, sigma.gamma)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    
    param.names <- c("beta0", 
                     "beta1",
                     "sigma",
                     "mu.gamma",
                     "sigma.gamma")
    
    t.param.names <- c("beta0*", 
                       "beta1",
                       "log.sigma",
                       "mu.gamma*",
                       "log.sigma.gamma")
    
    x.failed <- sort(the.xvec[ccodes == 1])
    mean.lx <- mean(logb(x.failed))
    low3rd <- logb(x.failed[1:as.integer(length(x.failed) / 3)])
    mean.low3rd <- mean(low3rd)
    
    model <- list(orig.param.names = param.names, 
                  t.param.names = t.param.names,
                  mean.lx = mean(logb(the.xvec)), 
                  f.origparam = f.origparam,
                  mean.lx = mean.lx, 
                  mean.low3rd = mean.low3rd, 
                  cond.dist = rfl.dist.map(cond.dist), 
                  fl.dist = rfl.dist.map(fl.dist),
                  distribution = paste("FatigueLimit = ", cond.dist, 
                                       ",Conditional = ", fl.dist, sep = ""))
    
    rfl.start <- function(data.ld) {
        ccodes <- censor.codes(data.ld)
        failed <- (ccodes == 1)
        xvals <- xmat(data.ld)[, 1]
        x.failed <- xvals[failed]
        y.failed <- logb(Response(data.ld))[failed]
        maxx <- logb(min(x.failed))
        minx <- logb(0.75 * min(xvals))
        mugamma.vec <- seq(minx, minx + 0.99 * (maxx - minx), length = 20)
        beta0.vec <- mugamma.vec
        beta1.vec <- mugamma.vec
        sigma.vec <- mugamma.vec
        sdgamma.vec <- mugamma.vec
        mse.vec <- mugamma.vec
        
        for (i in 1:20) {
            
            mugamma <- mugamma.vec[i]
            lsout <- lsfit(logb(x.failed - exp(mugamma)), y.failed)
            lin.coefs <- lsout$coef
            beta1.vec[i] <- lin.coefs[2]
            beta0.vec[i] <- lin.coefs[1]
            mse.vec[i] <- sum((lsout$residuals)^2)/(length(y.failed) - 2)
            sigma.vec[i] <- sqrt(mse.vec[i])
            sdgamma.vec[i] <- (logb(min(x.failed)) - mugamma) / 2
            if (sdgamma.vec[i] < 0.25) sdgamma.vec[i] <- 0.25
            if (sigma.vec[i] < 0.25) sigma.vec[i] <- 0.25
            
        }
        which <- (rank(mse.vec) == 1)
        theta.start <- c(beta0.vec[which], 
                         beta1.vec[which], 
                         sigma.vec[which],
                         mugamma.vec[which], 
                         sdgamma.vec[which])
        
        print(theta.start)
        return(theta.start)
    }
    
    if (is.null(theta.start)) theta.start <- rfl.start(data.ld)
    
    thetax <- f.tranparam(theta.start, model)
    thetay <- f.origparam(thetax, model)
    cat("beginning gmle rflm.loglike\n")
    
    gmle.out <- gmle(log.like = rflm.loglike, 
                     data.ld = data.ld, 
                     theta.start = theta.start, 
                     model = model, 
                     special.stuff = NULL, 
                     debug1 = debug1, 
                     t.param.names = t.param.names, 
                     f.tranparam = f.tranparam,
                     f.origparam = f.origparam,...)
    
    cat("after gmle rflm.loglike\n")
    gmle.out$func.call <- as.character(match.call())
    oldClass(gmle.out) <- c("rflm.mle", "gmle.out")
    MysetOldClass(attr(gmle.out, "class"))
    return(gmle.out)
    
}
