#' Title
#'
#' @param data.ld 
#' @param debug1 
#' @param theta.start 
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
#' summary(at7987.ld)
#' 
#' at7987.bisa.gmle.out <- bisa.mle(at7987.ld)
#' }
bisa.mle <-
function (data.ld,debug1= F, theta.start = NULL) 
{
    options(digits = 5)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    f.origparam <- function(thetatran, model) {
        tp1 <- exp(thetatran[1])
        beta <- exp(thetatran[2])
        pmiddle <- (model$p2 + model$p1)/2
        theta <- tp1/qbisa(pmiddle, scale = 1, shape = beta)
        thetaorig <- c(theta, beta)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    f.origparam2 <- function(thetatran, model) {
        tp1 <- exp(thetatran[1])
        tp2 <- exp(thetatran[2])
        if (tp1 > tp2) 
            return(rep(NA, length(thetatran)))
        zp1 <- qnorm(model$p1)
        zp2 <- qnorm(model$p2)
        theta <- (zp2 * tp1 * sqrt(tp2) - zp1 * tp2 * sqrt(tp1))/(zp2 * 
            sqrt(tp2) - zp1 * sqrt(tp1))
        beta <- (1/zp1) * (sqrt(tp1/theta) - sqrt(theta/tp1))
        thetaorig <- c(theta, beta)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    probs <- cdfest(data.ld)$prob
    p1 <- min(probs[probs > 0])/2
    p2 <- 0.9 * max(probs)
    orig.param.names <- c("theta", "beta")
    t.param.names <- c("logtp1", "logbeta")
    model <- list(p1 = p1, p2 = p2, distribution = "bisa", t.param.names = t.param.names, 
        orig.param.names = orig.param.names)
    if (is.null(theta.start)) {
        ls.gmle.out <- ls.mle(data.ld, distribution = "Lognormal")
        mu <- ls.gmle.out$origparam["location"]
        sigma <- ls.gmle.out$origparam["scale"]
        pmiddle <- (model$p2 + model$p1)/2
        logtp1 <- qnorm(pmiddle, mu, sigma)
        theta.start <- c(logtp1, 1)
    }
    gmle.out <- gmle(data.ld = data.ld, log.like = general.dist.log.like, 
        theta.start = theta.start, model = model, f.origparam = f.origparam, 
        t.param.names = t.param.names, orig.param.names = orig.param.names)
    return(gmle.out)
}
