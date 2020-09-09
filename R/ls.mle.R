#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param debug1 
#' @param theta.start 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1, 
#'                                 censor.column = 3,
#'                                 time.units = "Kilometers")
#'                                 
#' SA.weibull.gmle <- ls.mle(ShockAbsorber.ld, 
#'                           distribution = "Weibull")
#' }
ls.mle <-
function (data.ld, distribution = "weibull",debug1= 0, theta.start = NULL,
    ...)
{
    options(digits = 5)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    func.call <- match.call()
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    failures <- !(the.censor.codes == 2 | the.censor.codes ==
        0)
    pcensor <- (0.7 * sum(the.case.weights[failures]))/sum(the.case.weights)
    model <- list(distribution = distribution, pcensor = pcensor,
        form = NULL)
    f.tranparam <- function(theta, model) {
        sigma <- theta[2]
        logsigma <- logb(sigma)
        the.quant <- theta[1] + quant(model$pcensor, model$distribution) *
            sigma
        thetatran <- c(the.quant, logsigma)
        names(thetatran) <- model$t.param.names
        return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
        sigma <- exp(thetatran[2])
        mu <- thetatran[1] - quant(model$pcensor, model$distribution) *
            sigma
        thetaorig <- c(mu, sigma)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0)
    orig.param.names <- c("location", "scale")
    t.param.names <- c(paste("logt", format(signif(pcensor, 2)),
        sep = ""), "log(scale)")
    model$orig.param.names <- orig.param.names
    model$t.param.names <- t.param.names
    if (is.null(theta.start)) {
        theta.start <- mlest(data.ld, distribution)$theta.hat
    }
    gmle.out <- gmle(log.like = ls.log.like, data.ld = data.ld,
        theta.start = theta.start, model = model, f.origparam = f.origparam,
        f.tranparam = f.tranparam, t.param.names = t.param.names,
        orig.param.names = orig.param.names,debug1= debug1)
    theta.hat <- gmle.out$est.out$x
    mu <- theta.hat[1]
    sigma <- exp(theta.hat[2])
    parameters <- list(mu = mu, sigma = sigma)
    gmle.out$parameters <- parameters
    gmle.out$func.call <- func.call
    return(gmle.out)
}
