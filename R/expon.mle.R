expon.mle <-
function (data.ld,debug1= F, theta.start = NULL)
{
    y <- Response(data.ld)
    number.cases <- nrow(y)
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    options(digits = 5)
    assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    f.origparam <- function(thetatran, model) {
        scale <- exp(thetatran[1])
        thetaorig <- c(scale)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    orig.param.names <- c("scale")
    t.param.names <- c("log(scale)")
    model <- list(distribution = "Exponential", orig.param.names = orig.param.names,
        t.param.names = t.param.names)
    if (is.null(theta.start)) {
        theta.start <- sum(y[, 1] * the.case.weights)/sum(the.case.weights)
    }
    theta.start <- c(logb(theta.start))
    gmle.out <- gmle(data.ld = data.ld, log.like = exp.log.like,
        theta.start = theta.start, model = model, f.origparam = f.origparam,
        t.param.names = t.param.names, orig.param.names = orig.param.names,
       debug1= debug1)
    theta.hat <- gmle.out$est.out$x
    scale <- exp(theta.hat[1])
    return(gmle.out)
}
