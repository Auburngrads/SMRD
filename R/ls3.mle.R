ls3.mle <-
function (data.ld, distribution, relationship,debug1= 0, theta.start = NULL,
    explan.vars = 1:ncol(the.xmat), fixed.param.list = NULL,
    ...)
{
    options(digits = 5)
    func.call <- match.call()
    the.xmat <- xmat(data.ld)
    if (!is.null(the.xmat)) {
        if (length(relationship) != 1)
            stop("Relationship must be of length 1")
      box.cox.power <- "Box-Cox"
      relationship <- set.relationship.power(relationship,
            box.cox.power)
        xmat(data.ld) <- f.relationship(xmat(data.ld), relationship)
        the.xmat <- xmat(data.ld)
        if (ncol(the.xmat[, explan.vars, drop = F]) > 1)
            stop("Simple regression only")
        xbar <- mean(the.xmat)
  } else xbar <- NULL
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    failures <- !(the.censor.codes == 2 | the.censor.codes ==
        0)
    pcensor <- (0.7 * sum(the.case.weights[failures]))/sum(the.case.weights)
    model <- list(distribution = distribution, pcensor = pcensor,
        form = NULL, xbar = xbar)
    f.tranparam <- function(theta, model) {
        length.theta <- length(theta)
        sigma <- theta[length.theta]
        logsigma <- logb(sigma)
        if (is.null(model$xbar)) {
            mu <- theta[1]
            the.quant <- mu + quant(model$pcensor, model$distribution) *
                sigma
            thetatran <- c(the.quant, logsigma)
      } else {
            the.slope <- theta[2]
            mu <- theta[1] + the.slope * model$xbar
            the.quant <- mu + quant(model$pcensor, model$distribution) *
                sigma
            thetatran <- c(the.quant, the.slope, logsigma)
        }
        names(thetatran) <- model$t.param.names
        return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
        length.theta <- length(thetatran)
        sigma <- exp(thetatran[length.theta])
        if (is.null(model$xbar)) {
            mu <- thetatran[1] - quant(model$pcensor, model$distribution) *
                sigma
            thetaorig <- c(mu, sigma)
      } else {
            the.slope <- thetatran[2]
            the.intercept <- thetatran[1] - the.slope * model$xbar -
                quant(model$pcensor, model$distribution) * sigma
            thetaorig <- c(the.intercept, the.slope, sigma)
        }
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    if (!is.null(the.xmat)) {
        orig.param.names <- c("intercept", "slope", "scale")
        t.param.names <- c(paste("logt", format(signif(pcensor,
            2)), sep = ""), "slope", "log(scale)")
  } else {
        orig.param.names <- c("location", "scale")
        t.param.names <- c(paste("logt", format(signif(pcensor,
            2)), sep = ""), "log(scale)")
    }
    if (is.null(theta.start)) {
        if (!is.null(the.xmat)) {
            theta.start <- mlestlev(data.ld, distribution, explan.vars = explan.vars)$theta.hat
      } else {
            theta.start <- mlestlev(data.ld, distribution)$theta.hat
        }
    }
    cat("Start values\n")
    print(theta.start)
    model$orig.param.names <- orig.param.names
    model$t.param.names <- t.param.names
    gmle.out <- gmle(log.like = ls3.log.like, data.ld = data.ld,
        theta.start = theta.start, model = model, fixed.param.list = fixed.param.list,
        f.origparam = f.origparam, f.tranparam = f.tranparam,
        t.param.names = t.param.names, orig.param.names = orig.param.names,
       debug1= debug1)
    theta.hat <- gmle.out$est.out$x
    if (!is.null(the.xmat)) {
        intercept <- theta.hat[1]
        slope <- theta.hat[2]
        sigma <- exp(theta.hat[3])
        parameters <- list(intercept = intercept, slope = slope,
            sigma = sigma)
        reduced.xmat.names <- dimnames(the.xmat)[[2]]
        gmle.out$title <- paste(get.data.title(data.ld), "\n",
            paste(reduced.xmat.names, name.relationship(relationship,
                allow = T), sep = "", collapse = ", "), paste(", Dist:",
                distribution, sep = ""))
  } else {
        mu <- theta.hat[1]
        sigma <- exp(theta.hat[2])
        parameters <- list(mu = mu, sigma = sigma)
        reduced.xmat.names <- ""
        gmle.out$title <- paste(get.data.title(data.ld), "\n",
            paste(reduced.xmat.names, sep = "", collapse = ", "),
            paste(", Dist:", distribution, sep = ""))
    }
    gmle.out$parameters <- parameters
    gmle.out$func.call <- func.call
    return(gmle.out)
}
