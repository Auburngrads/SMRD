use.rate.full.mle <-
function (field.data.ld, lab.data.list, key.time,debug1= 0,
    lab.time.units = "Test Cycle", Wear.distribution.cycles = "lognormal",
    Wear.distribution.use.rate = "lognormal", Crack.distribution.cycles = "lognormal",
    Crack.distribution.use.rate = "lognormal", field.time.units = "Days",
    theta.start = NULL, correlation.model, fixed.rho, like.method = "density",
    plot.iter = c(1e+10), ...)
{
    options(digits = 5)
    if (correlation.model == "FixedRho") {
        if (missing(fixed.rho))
            stop("When correlation.model is FixedRho, you must provide argument fixed.rho")
        attr(correlation.model, "fixed.rho") <- fixed.rho
    }
    func.call <- match.call()
    list.data.ld <- list(field = field.data.ld, lab = lab.data.list)
    assign(envir = .frame0, inherits = !TRUE,"list.data.ld", list.data.ld)
    cat("Finding lab ML estimates\n")
    Crack.lab.theta.hat <- mlest(lab.data.list[["Crack"]], Crack.distribution.cycles)$theta.hat
    Wear.lab.theta.hat <- mlest(lab.data.list[["Wear"]], Wear.distribution.cycles)$theta.hat
    print(Wear.lab.theta.hat)
    print(Crack.lab.theta.hat)
    lab.parameters <- c(Crack.lab.mu = Crack.lab.theta.hat["mu"],
        Crack.lab.sigma = Crack.lab.theta.hat["sigma"], Wear.lab.mu = Wear.lab.theta.hat["mu"],
        Wear.lab.sigma = Wear.lab.theta.hat["sigma"])
    special.stuff <- list(plot.iter = plot.iter, lab.parameters = lab.parameters)
    distributions <- c(Crack.distribution.cycles = Crack.distribution.cycles,
        Crack.distribution.use.rate = Crack.distribution.use.rate,
        Wear.distribution.cycles = Wear.distribution.cycles,
        Wear.distribution.use.rate = Wear.distribution.use.rate)
    model <- list(distributions = distributions, lab.time.units = lab.time.units,
        field.time.units = field.time.units, correlation.model = correlation.model,
        like.method = like.method, key.time = key.time)
    f.tranparam <- function(theta, model) {
        TheFieldParameters <- FieldParameters2(theta)
        zCrack <- (model$key.time - TheFieldParameters$Field.muCrack)/TheFieldParameters$Field.sigmaCrack
        Crack.log.sigma.Field <- log(TheFieldParameters$Field.sigmaCrack)
        zWear <- (model$key.time - TheFieldParameters$Field.muWear)/TheFieldParameters$Field.sigmaWear
        Wear.log.sigma.Field <- log(TheFieldParameters$Field.sigmaWear)
        Crack.mu.cycles <- theta["Crack.mu.cycles"]
        Crack.log.sigma.cycles <- log(theta["Crack.sigma.cycles"])
        Wear.mu.cycles <- theta["Wear.mu.cycles"]
        Wear.log.sigma.cycles <- log(theta["Wear.sigma.cycles"])
        ztran <- function(rho) {
            log((1 + rho)/(1 - rho))
        }
        zRhoRR <- ztran(theta["RhoRR"])
        thetatran <- c(zCrack, Crack.log.sigma.Field, zWear,
            Wear.log.sigma.Field, Crack.mu.cycles, Crack.log.sigma.cycles,
            Wear.mu.cycles, Wear.log.sigma.cycles, zRhoRR)
        names(thetatran) <- model$t.param.names
        return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
        zinvtran <- function(z) {
            expz <- exp(z)
            (expz - 1)/(expz + 1)
        }
        Crack.mu.cycles <- (thetatran["Crack.mu.cycles"])
        Crack.sigma.cycles <- exp(thetatran["Crack.log.sigma.cycles"])
        Wear.mu.cycles <- (thetatran["Wear.mu.cycles"])
        Wear.sigma.cycles <- exp(thetatran["Wear.log.sigma.cycles"])
        RhoRR <- zinvtran(thetatran["zRhoRR"])
        Crack.sigma.Field <- exp(thetatran["Crack.log.sigma.Field"])
        Crack.sigma.use.rate <- Crack.sigma.Field^2 - Crack.sigma.cycles^2
        Crack.mu.Field <- model$key.time - (thetatran["zCrack"]) *
            Crack.sigma.Field
        Crack.mu.use.rate <- Crack.mu.cycles - Crack.mu.Field
        Wear.sigma.Field <- exp(thetatran["Wear.log.sigma.Field"])
        Wear.sigma.use.rate <- Wear.sigma.Field^2 - Wear.sigma.cycles^2
        Wear.mu.Field <- model$key.time - (thetatran["zWear"]) *
            Wear.sigma.Field
        Wear.mu.use.rate <- Wear.mu.cycles - Wear.mu.Field
        thetaorig <- c(Crack.mu.use.rate, Crack.sigma.use.rate,
            Wear.mu.use.rate, Wear.sigma.use.rate, Crack.mu.cycles,
            Crack.sigma.cycles, Wear.mu.cycles, Wear.sigma.cycles,
            RhoRR)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    model$lab.param.names <- c("Crack.mu.cycles", "Crack.sigma.cycles",
        "Wear.mu.cycles", "Wear.sigma.cycles")
    model$orig.param.names <- c("Crack.mu.use.rate", "Crack.sigma.use.rate",
        "Wear.mu.use.rate", "Wear.sigma.use.rate", "Crack.mu.cycles",
        "Crack.sigma.cycles", "Wear.mu.cycles", "Wear.sigma.cycles",
        "RhoRR")
    model$t.param.names <- c("zCrack", "Crack.log.sigma.Field",
        "zWear", "Wear.log.sigma.Field", "Crack.mu.cycles", "Crack.log.sigma.cycles",
        "Wear.mu.cycles", "Wear.log.sigma.cycles", "zRhoRR")
    if (is.null(theta.start)) {
        cat("Computing starting values for use rate parameters\n")
        field.data.list <- mfm.to.ld(field.data.ld)
        Crack.field.theta.hat <- mlest(field.data.list[["Crack"]],
            Crack.distribution.cycles)$theta.hat
        Wear.field.theta.hat <- mlest(field.data.list[["Wear"]],
            Wear.distribution.cycles)$theta.hat
        print(Crack.field.theta.hat)
        print(Wear.field.theta.hat)
        Crack.mu.use.rate <- (Crack.lab.theta.hat["mu"] - Crack.field.theta.hat["mu"])
        Wear.mu.use.rate <- (Wear.lab.theta.hat["mu"] - Wear.field.theta.hat["mu"])
        switch(correlation.model, UR.UR.independent = {
            cov <- 0
            Crack.sigma.use.rate <- max(1e-04, sqrt(Crack.field.theta.hat["sigma"]^2 -
                Crack.lab.theta.hat["sigma"]^2) - 2 * cov)
            Wear.sigma.use.rate <- max(1e-04, sqrt(Wear.field.theta.hat["sigma"]^2 -
                Wear.lab.theta.hat["sigma"]^2) - 2 * cov)
        }, Wear.Ratio.independent = {
            rho <- 0.2
            WearRoot <- quadroot(a = 1, b = 2 * rho * Wear.lab.theta.hat["sigma"],
                c = Wear.lab.theta.hat["sigma"]^2 - Wear.field.theta.hat["sigma"]^2)
            WearRoot2 <- -rho * Wear.lab.theta.hat["sigma"] +
                sqrt(Wear.field.theta.hat["sigma"]^2 + Wear.lab.theta.hat["sigma"] *
                  (rho^2 - 1))
            print(c(WearRoot, WearRoot2))
            Wear.sigma.use.rate <- max(1e-04, WearRoot)
            Crack.sigma.use.rate <- Wear.sigma.use.rate/rho
        }, Crack.Ratio.independent = {
            rho <- 0.5
            WearRoot <- quadroot(a = 1, b = 2 * rho * Wear.lab.theta.hat["sigma"],
                c = Wear.lab.theta.hat["sigma"]^2 - Wear.field.theta.hat["sigma"]^2)
            WearRoot2 <- -rho * Wear.lab.theta.hat["sigma"] +
                sqrt(Wear.field.theta.hat["sigma"]^2 + Wear.lab.theta.hat["sigma"] *
                  (rho^2 - 1))
            print(c(WearRoot, WearRoot2))
            Wear.sigma.use.rate <- max(1e-04, WearRoot)
            Crack.sigma.use.rate <- Wear.sigma.use.rate * rho
        }, FixedRho = {
            rho <- attr(correlation.model, "fixed.rho")
            WearRoot <- quadroot(a = 1, b = 2 * rho * Wear.lab.theta.hat["sigma"],
                c = Wear.lab.theta.hat["sigma"]^2 - Wear.field.theta.hat["sigma"]^2)
            WearRoot2 <- -rho * Wear.lab.theta.hat["sigma"] +
                sqrt(Wear.field.theta.hat["sigma"]^2 + Wear.lab.theta.hat["sigma"] *
                  (rho^2 - 1))
            print(c(WearRoot, WearRoot2))
            Wear.sigma.use.rate <- max(1e-04, WearRoot)
            CrackRoot <- quadroot(a = 1, b = 2 * rho * Crack.lab.theta.hat["sigma"],
                c = Crack.lab.theta.hat["sigma"]^2 - Crack.field.theta.hat["sigma"]^2)
            CrackRoot2 <- -rho * Crack.lab.theta.hat["sigma"] +
                sqrt(Crack.field.theta.hat["sigma"]^2 + Crack.lab.theta.hat["sigma"] *
                  (rho^2 - 1))
            print(c(CrackRoot, CrackRoot2))
            Crack.sigma.use.rate <- max(1e-04, CrackRoot)
        }, {
            stop("Unrecognized correlation.model")
        })
        theta.start <- c(Crack.mu.use.rate, Crack.sigma.use.rate,
            Wear.mu.use.rate, Wear.sigma.use.rate)
        names(theta.start) <- model$orig.param.names
    }
    cat("Start values\n")
    print(theta.start)
    tr.theta.start <- f.tranparam(theta.start, model)
    cat("Transformed parameters\n")
    print(tr.theta.start)
    test.theta.start <- f.origparam(tr.theta.start, model)
    cat("Test orig parameters\n")
    print(test.theta.start)
    test.use.rate.model <- get.use.rate.model(use.rate.parameters = theta.start,
        lab.parameters = lab.parameters, lab.time.units = lab.time.units,
        field.time.units = field.time.units, distributions = distributions,
        correlation.model = correlation.model)
    assign(envir = .frame0, inherits = !TRUE,"test.use.rate.model", test.use.rate.model)
    gmle.out <- gmle(log.like = use.rate.log.like, data.ld = field.data.ld,
        theta.start = theta.start, model = model, f.tranparam = f.tranparam,
        f.origparam = f.origparam, special.stuff = special.stuff,
       debug1= debug1, max.fcal = 99, rfc.tol = 1e-10, xc.tol = 1e-10,
        )
    gmle.out$distributions <- distributions
    gmle.out$kodet <- c(2, 2, 2, 2)
    gmle.out$use.rate.model <- get.use.rate.model(use.rate.parameters = gmle.out$origparam,
        lab.parameters = lab.parameters, lab.time.units = lab.time.units,
        field.time.units = field.time.units, distributions = distributions,
        correlation.model = correlation.model)
    gmle.out$func.call <- func.call
    gmle.out$field.data.ld <- field.data.ld
    gmle.out$lab.data.list <- lab.data.list
    oldClass(gmle.out) <- c("use.rate.model.out", oldClass(gmle.out))
    MysetOldClass(attr(gmle.out, "class"))
    return(gmle.out)
}
