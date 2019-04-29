print.mcf <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    citype = "logtran", digits = GetSMRDDefault("SMRD.DigitsPrinted"),
    ...)
{
    cat(paste("\n\nNonparametric MCF estimate from ", get.data.title(x),
        "data\n"))
    old <- options(digits = digits)
    on.exit(options(old))
    normal.quantile <- qnorm(1 - (1 - conf.level)/2)
    xmuhat <- x$muHat
    sdxmuhat <- sqrt(x$VarHat)
    if (citype == "logtran") {
        factor <- exp((normal.quantile * sdxmuhat)/xmuhat)
        lcl <- xmuhat/factor
        ucl <- xmuhat * factor
    }
    else {
        factor <- normal.quantile * sdxmuhat
        lcl <- xmuhat - factor
        ucl <- xmuhat + factor
    }
    the.table <- cbind(x$tuniq, xmuhat, sdxmuhat, lcl,
        ucl)
    dimnames(the.table) <- list(rep(" ", nrow(the.table)), c("Time",
        "MCFhat", "SE MCFhat", "Lower", "Upper"))
    cat(paste("\n                    ", percent.conf.level(conf.level),
        "Confidence Interval\n"))
    print(the.table)
}
