#' Title
#'
#' @param data.ld 
#' @param start.distribution 
#' @param theta.start 
#' @param debug1 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' at7987.ld <- frame.to.ld(at7987, 
#'                          response.column = 1,
#'                          censor.column = 2, 
#'                          case.weight.column = 3,
#'                          time.units = "Kilocycles")
#'                          
#' at7987.egeng.gmle.out <- egeng.mle(at7987.ld)
#' }
egeng.mle <-
function (data.ld, start.distribution = "Weibull", theta.start = NULL, debug1 = F) 
{
    options(digits = 5)
    f.tranparam <- function(theta, model) {
        mu <- theta[1]
        sigma <- theta[2]
        lambda <- theta[3]
        logtp1 <- qegengl(model$p1, mu, sigma, lambda)
        logtp2 <- qegengl(model$p2, mu, sigma, lambda)
        lambdastar = log((lambda - model$lambdastarlower)/(model$lambdastarupper - 
            lambda))
        thetatran <- c(logtp1, logtp2, lambdastar)
        names(thetatran) <- model$t.param.names
        return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
        logtp1 <- thetatran[1]
        logtp2 <- thetatran[2]
        lambdastar <- thetatran[3]
        explambdastar <- exp(lambdastar)
        lambda <- (model$lambdastarupper * explambdastar + model$lambdastarlower)/(explambdastar + 
            1)
        logwp2 <- qegengl(model$p2, 0, 1, lambda)
        logwp1 <- qegengl(model$p1, 0, 1, lambda)
        diff <- (logwp2 - logwp1)
        if (abs(diff) < 1e-300) 
            diff <- 10^(-300)
        mu <- (logwp2 * logtp1 - logwp1 * logtp2)/diff
        sigma <- (logtp2 - logtp1)/diff
        thetaorig <- c(mu, sigma, lambda)
        names(thetaorig) <- model$orig.param.names
        if (any(thetaorig == Inf)) 
            browser()
        return(thetaorig)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    probs <- cdfest(data.ld)$prob
    p1 <- min(probs[probs > 0])/2
    the.last <- length(probs)
    p2 <- max(probs)
    if (p2 > 0.5) 
        p2 <- (probs[the.last] + probs[the.last - 1])/2
    orig.param.names <- c("mu", "sigma", "lambda")
    t.param.names <- c("logtp1", "logtp2", "lambdastar")
    model <- list(distribution = "egeng", t.param.names = t.param.names, 
        orig.param.names = orig.param.names, p1 = p1, p2 = p2, 
        lambdastarupper = 2, lambdastarlower = -2)
    if (is.null(theta.start)) {
        ls.gmle.out <- ls.mle(data.ld, distribution = start.distribution)
        mu <- ls.gmle.out$origparam["location"]
        sigma <- ls.gmle.out$origparam["scale"]
        switch(generic.distribution(start.distribution), weibull = {
            lambda <- 1
        }, lognormal = {
            lambda <- 0
        }, stop("Use only Weibull or lognormal for start.distribution"))
        theta.start <- c(mu, sigma, lambda)
    }
    print(theta.start)
    theta.start.star <- f.tranparam(theta.start, model)
    print(theta.start.star)
    theta.start.test <- f.origparam(theta.start.star, model)
    print(theta.start.test)
    gmle.out <- gmle(log.like = egeng.log.like, data.ld = data.ld, 
        theta.start = theta.start, model = model, f.tranparam = f.tranparam, 
        f.origparam = f.origparam, t.param.names = t.param.names, 
        orig.param.names = orig.param.names)
    invisible(gmle.out)
}
