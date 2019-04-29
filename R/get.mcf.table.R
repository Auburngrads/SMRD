get.mcf.table <-
function (mcf.out, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    citype = "notran", print.table = F)
{
    normal.quantile <- qnorm(1 - (1 - conf.level)/2)
    muHat <- mcf.out$muHat
    sdmuHat <- sqrt(mcf.out$VarHat)
    if (citype == "logtran") {
        factor <- exp((normal.quantile * sdmuHat)/muHat)
        lcl <- muHat/factor
        ucl <- muHat * factor
    } else {
        factor <- normal.quantile * sdmuHat
        lcl <- muHat - factor
        ucl <- muHat + factor
    }
    the.table <- cbind(mcf.out$tuniq, muHat, sdmuHat, lcl, ucl)
    dimnames(the.table) <- list(rep(" ", nrow(the.table)), c("Time",
        "mu-hat", "SE mu-hat", "Lower", "Upper"))
    class(the.table) <- "mcf.table"
    attr(the.table, "conf.level") <- conf.level
    attr(the.table, "title") <- mcf.out$title
    if (print.table)
        cat(paste("\n                    ", percent.conf.level(conf.level),
            "Confidence Interval\n"))
    return(the.table)
}
