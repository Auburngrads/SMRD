SummarizeMultiple.SimRMDOverTime <-
function (RMD.pv, RMD.plan.vec, EvaluateEndTimes, begin.time = 1,
    number.sim = 200, p = 0.1, plot.stuff = F, print.stuff = F,
    time.vec = 1:20, report.type = "l", lty.vec = c(1, 3, 4,
        5, 6), col.vec = c(1, 2, 3, 4, 7), xlim = c(NA, NA),
    ylim = c(NA, NA))
{
    results.matrix <- matrix(NA, ncol = length(EvaluateEndTimes),
        nrow = length(time.vec))
    ylab <- paste("SD of", p, "Quantile Estimates")
    the.plan.string.vec <- rep("", length(RMD.plan.vec))
    RMD.pv.name <- deparse(substitute(RMD.pv))
    for (i in 1:length(RMD.plan.vec)) {
        the.plan <- get(envir = .frame0, RMD.plan.vec[i])
        time.units <- attr(the.plan, "time.units")
        the.plan.string.vec[i] <- getPlanString(the.plan)
        the.plan.string.name <- getPlanString(the.plan, for.name = T)
        for (j in 1:length(EvaluateEndTimes)) {
            observation.time.range <- c(begin.time, EvaluateEndTimes[j])
            results.object.name <- paste("PV", RMD.pv.name, "PLAN",
                RMD.plan.vec[i], the.plan.string.name, "End",
                EvaluateEndTimes[j], "B", number.sim, sep = "")
            if (exists(results.object.name)) {
                cat("Recovering", results.object.name, "\n")
                plan.results <- get(envir = .frame0, results.object.name)
}           else {
                stop(paste(c("Results", results.object.name,
                  "not on the database"),collapse = " "))
            }
            results.matrix[, j] <- SimulatedQuantileSummary(plan.results,
                p = p, plot.stuff = plot.stuff, time.vec = time.vec)$standard.deviations
        }
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- range(time.vec)[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- range(results.matrix)[yrna]
        if (i == 1) {
            plot.paper(xlim, ylim, grids = F, xlab = time.units,
                ylab = ylab)
            title(paste("Standard Deviations of ML Estimates \nof the",
                p, "Degradation Quantile"))
        }
        for (j in 1:length(EvaluateEndTimes)) {
            lines(time.vec, results.matrix[, j], type = report.type,
                lty = lty.vec[i], col = col.vec[i])
            if (i == 1)
                text(x.loc(0.94), results.matrix[nrow(results.matrix),
                  j], paste("After", as.character(EvaluateEndTimes[j])))
        }
    }
    legend(x.loc(0.01), y.loc(0.98), paste("Plan:", the.plan.string.vec),
        lty = lty.vec[1:length(the.plan.string.vec)],
        col = col.vec[1:length(the.plan.string.vec)],y.intersp = 0.675)
}
