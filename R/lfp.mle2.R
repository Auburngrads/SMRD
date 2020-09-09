lfp.mle2 <-
function (data.ld, distribution, dump = F)
{
    options(digits = 5)
    assign(envir = .frame0, inherits = !TRUE,"dump", dump)
    theResponse <-Response(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    special.stuff <- list(bigtime = max(logb(theResponse)))
    model <- list(distribution = distribution)
    param.names <- c("location", "scale", "prob.lfp")
    t.param.names <- c("location", "log(scale)", "logit(prob.lfp*PhiC)")
    mu.start <- mean(logb(theResponse)[the.censor.codes == 1])
    sigma.start <- sqrt(var(logb(theResponse)[the.censor.codes ==
        1]))
    p.start <- (sum(the.case.weights[the.censor.codes == 1])/sum(the.case.weights[the.censor.codes ==
        1]))/90
    mu.start <- 4.05
    sigma.start <- 2.12
    p.start <- 0.00827
    theta.start <- c(mu.start, logb(sigma.start), qlogis(p.start *
        wqmf.phibf((max(logb(theResponse)) - mu.start)/sigma.start,
            distribution)))
    gmle.out <- gmle(log.like = lfp.log.like2, mle.data = data.ld,
        theta.start = theta.start, model = model, special.stuff = special.stuff,
        t.param.names = t.param.names)
    print.gmle(gmle.out)
    theta.hat <- gmle.out$est.out$x
    mu <- theta.hat[1]
    sigma <- exp(theta.hat[2])
    prob.lfp <- plogis(theta.hat[3])/wqmf.phibf((max(logb(theResponse)) -
        mu)/sigma, distribution)
    parameters <- list(mu = mu, sigma = sigma, prob.lfp = prob.lfp)
    gmle.out$parameters <- parameters
    return(gmle.out)
}
