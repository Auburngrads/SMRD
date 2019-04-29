asym.sample.size <-
function (plan.values, quantile.of.interest, Rvalue = NULL, HalfWidth = NULL,
    censor.time, fraction.failing, conf.level = 0.95)
{
    time.units <- plan.values$time.units
    mu <- plan.values$mu
    sigma <- plan.values$sigma
    distribution <- generic.distribution(plan.values$distribution)
    if (is.logdist(distribution)) {
        if (!missing(censor.time))
            log.censor.time <- logb(censor.time)
        if (is.null(Rvalue))
            stop("Must specify Rvalue")
        xRvalue <- logb(Rvalue)
  } else {
        if (!missing(censor.time))
            log.censor.time <- censor.time
        xRvalue <- (HalfWidth)
        if (is.null(HalfWidth))
            stop("Must specify HalfWidth")
    }
    if (missing(fraction.failing)) {
        if (missing(censor.time))
            stop("Need to specify either censor.time or fraction.failing")
        censoring.type <- "Type I"
        std.log.censor.time <- (log.censor.time - mu)/sigma
        std.quantile.of.interest <- quant(quantile.of.interest,
            distribution)
        fraction.failing <- wqmf.phibf(std.log.censor.time, distribution)
  } else {
        if (!missing(censor.time))
            stop("Cannot specify both censor.time or fraction.failing")
        censoring.type <- "Type II"
        if (fraction.failing < 0 || fraction.failing > 1)
            stop(paste("fraction.failing", fraction.failing,
                " must be greater than 0 and less than or equal to 1"))
        if (fraction.failing < 1) {
            std.log.censor.time <- quant(fraction.failing, distribution)
            log.censor.time <- mu + std.log.censor.time * sigma
      } else {
            std.log.censor.time <- quant(0.99999999, distribution)
            log.censor.time <- mu + std.log.censor.time * sigma
        }
        std.quantile.of.interest <- quant(quantile.of.interest,
            distribution)
        if (is.logdist(distribution))
            censor.time <- exp(log.censor.time)
        else censor.time <- log.censor.time
    }
    if (numdist(distribution) == 2) {
        plan.string <- paste("\n\nModel: ", distribution, " distribution with eta=",
            format(exp(mu)), " and beta=", format(1/sigma), ".",
            sep = "")
  } else {
        plan.string <- paste("\n\nModel: ", distribution, " distribution with mu=",
            format(mu), " and sigma=", format(sigma), ".", sep = "")
    }
    if (censoring.type == "Type I") {
        censor.string <- paste("\nTest duration: censored at",
            format(censor.time, digits = 4), time.units, "with",
            format(100 * fraction.failing, digits = 3), "expected percent failing.\n")
  } else {
        censor.string <- paste("\nTest duration: terminated after",
            format(100 * fraction.failing, digits = 3), "percent failing.\n")
    }
    if (T) {
        if (is.logdist(distribution))
            my.title <- paste(plan.string, censor.string, "Sample size needed to give approximately a 50% chance of having\na confidence interval factor R that is less than ",
                Rvalue, "\nfor the ", quantile.of.interest, " quantile is: ",
                sep = "")
        else my.title <- paste(plan.string, censor.string, "Sample size needed to give approximatley a 50% chance of having\na confidence interval half-width D that is less than ",
            HalfWidth, "\nfor the ", quantile.of.interest, " quantile is: ",
            sep = "")
    }
    var.out <- ftavarvec(plan.values$distribution, std.log.censor.time)
    variance.factor <- (plan.values$sigma^2) * (var.out$v11 +
        2 * std.quantile.of.interest * var.out$v12 + (std.quantile.of.interest)^2 *
        var.out$v22)
    for (i in 1:length(conf.level)) {
        zvalue <- quant(1 - (1 - conf.level)/2, "normal")
        answer <- ((zvalue^2) * variance.factor)/(xRvalue^2)
    }
    cat(my.title, format(ceiling(answer)), "\n\n")
    invisible(answer)
}
