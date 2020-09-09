use.rate.log.like <-
function (thetain)
{
    iter.count <- get(envir = .frame0,  "iter.count") + 1
    assign(envir = .frame0, inherits = !TRUE,"iter.count", iter.count )
   debug1<- get(envir = .frame0,  "debug1")
    model <- get(envir = .frame0,  "model")
    list.data.ld <- get(envir = .frame0,  "list.data.ld")
    field.data.ld <- list.data.ld$field
    lab.data.list <- list.data.ld$lab
    f.origparam <- model$f.origparam
    distributions <- model$distributions
    like.method <- model$like.method
    theta.origparam <- f.origparam(thetain, model)
    special.stuff <- get(envir = .frame0,  "special.stuff")
    plot.iter <- special.stuff$plot.iter
    lab.parameters <- special.stuff$lab.parameters
    use.rate.model <- get.use.rate.model(use.rate.parameters = theta.origparam,
        lab.parameters = lab.parameters, lab.time.units = model$lab.time.units,
        field.time.units = model$field.time.units, distributions = distributions,
        correlation.model = model$correlation.model)
    do.plot <- any(plot.iter == iter.count)
    cex.fact <- 1
    Wear.sigma.use.rate <- theta.origparam["Wear.sigma.use.rate"]
    Crack.sigma.use.rate <- theta.origparam["Crack.sigma.use.rate"]
    if (any(c(Wear.sigma.use.rate, Crack.sigma.use.rate) < 1e-05))
        return(1e+10)
    if (is.logdist(distributions["Crack.distribution.cycles"])) {
        field.yresp <- log(Response(field.data.ld))
    }
    else {
        field.yresp <-Response(field.data.ld)
    }
    field.failure.mode <- failure.modes(field.data.ld)
    field.censor.codes <- censor.codes(field.data.ld)
    field.case.weights <- case.weights(field.data.ld)
    field.fail.Wear.part <- 0
    field.fail.Crack.part <- 0
    field.fail.censor.part <- 0
    if (debug1>= 4 && iter.count < 2)
        browser()
    wear.ones <- field.failure.mode == "Wear"
    crack.ones <- field.failure.mode == "Crack"
    field.censor.ones <- field.censor.codes == 2
    if (any(wear.ones))
        field.fail.Wear.part <- sum(field.case.weights[wear.ones] *
            use.rate.fail.contrib(yresp = field.yresp[wear.ones,
                1], failure.mode = "Wear", use.rate.model = use.rate.model,
                like.method = like.method))
    if (any(crack.ones))
        field.fail.Crack.part <- sum(field.case.weights[crack.ones] *
            use.rate.fail.contrib(yresp = field.yresp[crack.ones,
                1], failure.mode = "Crack", use.rate.model = use.rate.model,
                like.method = like.method))
    if (any(field.censor.ones))
        field.fail.censor.part <- sum(field.case.weights[field.censor.ones] *
            use.rate.rcen.contrib(yresp = field.yresp[field.censor.ones,
                1], use.rate.model))
    the.log.like <- field.fail.Wear.part + field.fail.Crack.part +
        field.fail.censor.part
    if (debug1> 0) {
        the.message <- paste("in ur", iter.count, paste(format(theta.origparam),
            collapse = ", "), format(use.rate.model$rho), format(the.log.like))
        cat("likelihood.parts", format(c(the.log.like, field.fail.Wear.part,
            field.fail.Crack.part, field.fail.censor.part)),
            "\n")
    }
    if (do.plot) {
        if (sum(par("mfrow")) > 2) {
            cex.fact <- 0.5
            hold.options <- SMRDOptions(SMRD.DateOnPlot = F)
            on.exit(SMRDOptions(hold.options))
        }
        else {
            cex.fact <- 1
        }
        plot.use.rate.model(use.rate.model, field.data.ld = field.data.ld,
            lab.data.list = lab.data.list, cex.fact = cex.fact,
            my.title = paste("Iteration", iter.count, "Loglikelihood=",
                format(the.log.like), "rho=", format(use.rate.model$rho)))
    }
    return(Uminus((the.log.like)))
}
