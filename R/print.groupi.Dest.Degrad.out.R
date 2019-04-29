print.groupi.Dest.Degrad.out <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    digits = GetSMRDDefault("SMRD.DigitsPrinted"),...)
{
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    distribution <- attr(x, "distribution")
    result.table <- cbind(stress = attr(x,
        "stress"), sample.size = attr(x,
        "sample.size"), intercept = attr(x,
        "intercept"), slope = attr(x, "slope"),
        se.slope = attr(x, "se.slope"),
        slope.lower = attr(x, "slope.lower"),
        slope.upper = attr(x, "slope.upper"))
    the.title <- get.data.title(attr(x,
        "data.ddd"))
    transformation.response <- attr(x, "transformation Response")
    transformation.time <- attr(x, "transformation.time")
    cat("\n", the.title, "Response transformation:", transformation.response,
        "\nTime transformation:", transformation.time, "\n")
    cat(distribution, "degradation distribution.\n\n", "Individual estimates and ",
        percent.conf.level(conf.level), "confidence intervals for the slope at each condition.\n\n")
    the.dim.names <- dimnames(result.table)
    the.dim.names[[1]] <- rep(" ", nrow(result.table))
    dimnames(result.table) <- the.dim.names
    print(result.table)
    invisible(result.table)
}
