mcf.table <-
function (data.rd, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    citype = "logtran", print.table = F)
{
    nprepair.out <- nprepair(data.rd)
    normal.quantile <- qnorm(1 - (1 - conf.level)/2)
    xmuhat <- nprepair.out$xmuhat
    sdxmuhat <- sqrt(nprepair.out$varxmu)
    if (citype == "logtran") {
        factor <- exp((normal.quantile * sdxmuhat)/xmuhat)
        lcl <- xmuhat/factor
        ucl <- xmuhat * factor
  } else {
        factor <- normal.quantile * sdxmuhat
        lcl <- xmuhat - factor
        ucl <- xmuhat + factor
    }
    the.table <- cbind(nprepair.out$utime, xmuhat, sdxmuhat,
        lcl, ucl)
    dimnames(the.table) <- list(rep(" ", nrow(the.table)), c("Time",
        "mu-hat", "SE mu-hat", "Lower", "Upper"))
    oldClass(the.table) <- "mcf.table"
    attr(the.table, "conf.level") <- conf.level
    attr(the.table, "title") <- data.rd$title
    if (print.table)
        cat(paste("\n                    ", percent.conf.level(conf.level),
            "Confidence Interval\n"))
    return(the.table)
}
