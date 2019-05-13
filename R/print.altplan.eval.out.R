#' @export
print.altplan.eval.out <-
function (x, details = F,...)
{
    cat("\nAccelerated Test Plan\n")
    alt.test.plan <- x$alt.test.plan
    accel.variable <- attr(alt.test.plan, "accelvar.names")
    the.frame <- data.frame(Level = alt.test.plan[[accel.variable]],
        Number = alt.test.plan$number.units, CensorTime = alt.test.plan$censor.times,
        pi = format(x$pi, digits = 3), PrFail = format(x$pfail,
            digits = 6), Efail = format(x$efail,
            digits = 3))
    the.frame <- the.frame[x$pi > 0, ]
    print(the.frame)
    if (!is.null(x$use.condition))
        cat("\nUse condition is", x$use.condition,
            x$alt.plan.values$accelvar.units,
            "\n")
    cat("\nTotal number of test units=", sum(alt.test.plan$number.units),
        "\n")
    for (ivec in 1:length(x$perc)) {
        if (is.logdist(x$distribution))
            is.log.message <- "log"
        else is.log.message <- ""
        asd <- x$asd[ivec]
        Rfact <- exp(1.96 * asd)
        cat("\nThe large sample approximate standard deviation\nof the",
            x$perc[ivec], is.log.message, "quantile at",
            x$use.condition, x$alt.plan.values$accelvar.units,
            " =", format(asd, digits = 4), "and the", "\ncorresponding 95% confidence precision factor of R=",
            format(Rfact, digits = 4), "\n")
    }
    x$alt.test.plan <- NULL
    x$alt.plan.values <- NULL
    if (details)
        print(x)
    invisible(the.frame)
}
