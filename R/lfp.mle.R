lfp.mle <-
function (data.ld, distribution, dump = F, theta.start = NULL)
{
    options(digits = 5)
    assign(envir = .frame0,  inherits = TRUE,"dump", dump)
    log.theResponse <- logb(Response(data.ld))
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    model <- list(distribution = distribution, form = "LFP")
    param.names <- c("location", "scale", "prob.lfp")
    t.param.names <- c("location", "log(scale)", "logit(prob.lfp)")
    mu.start <- mean(log.theResponse[the.censor.codes == 1])
    sigma.start <- sqrt(var(log.theResponse[the.censor.codes ==
        1]))
    p.start <- (sum(the.case.weights[the.censor.codes == 1])/sum(the.case.weights[the.censor.codes >
        0]))
    if (is.null(theta.start))
        theta.start <- c(mu.start, logb(sigma.start), qlogis(p.start))
    gmle.out <- gmle(log.like = lfp.log.like, mle.data = data.ld,
        theta.start = theta.start, model = model, t.param.names = t.param.names)
    theta.hat <- gmle.out$est.out$x
    mu <- theta.hat[1]
    sigma <- exp(theta.hat[2])
    prob.lfp <- plogis(theta.hat[3])
    parameters <- list(mu = theta.hat[1], sigma = exp(theta.hat[2]),
        prob.lfp = plogis(theta.hat[3]))
    gmle.out$parameters <- parameters
    return(gmle.out)
}
