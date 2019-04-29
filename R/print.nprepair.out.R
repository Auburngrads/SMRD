print.nprepair.out <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    citype = "logtran",...)
{
    normal.quantile <- qnorm(1 - (1 - conf.level)/2)
    xmuhat <- x$xmuhat
    sdxmuhat <- sqrt(x$varxmu)
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
    the.table <- cbind(x$utime, xmuhat, sdxmuhat,
        lcl, ucl)
    dimnames(the.table) <- list(rep(" ", nrow(the.table)), c("Time",
        "mu-hat", "SD Mu-hat", "Lower", "Upper"))
    cat("\n", x$data.rd$title, "\n")
    cat(paste("\n                    ", paste(floor(100 * conf.level +
        0.01), "%", sep = ""), "Confidence Interval\n"))
    print(the.table)
    invisible()
    return()
}
