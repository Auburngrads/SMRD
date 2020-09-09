ComputeSummarizeMultiple.SimRMDOverTime <-
function (RMD.pv, RMD.plan.vec, EvaluateEndTimes, begin.time = 1,
    number.sim = 200, p = 0.1, plot.stuff = F, print.stuff = F,
    time.vec = 1:20, report.type = "l", lty = 1)
{
    results.matrix <- matrix(NA, ncol = length(EvaluateEndTimes),
        nrow = length(time.vec))
    ylab <- paste("SD of", p, "Quantile Estimates")
    RMD.pv.name <- deparse(substitute(RMD.pv))
    for (i in 1:length(RMD.plan.vec)) {
        the.plan <- get(envir = .frame0, RMD.plan.vec[i])
        the.plan.string <- getPlanString(the.plan)
        the.plan.string.name <- getPlanString(the.plan, for.name = T)
        for (j in 1:length(EvaluateEndTimes)) {
            observation.time.range <- c(begin.time, EvaluateEndTimes[j])
            results.object.name <- paste("PV", RMD.pv.name, "PLAN",
                RMD.plan.vec[i], the.plan.string.name, "End",
                EvaluateEndTimes[j], "B", number.sim, sep = "")
            if (exists(results.object.name)) {
                cat("Recovering", results.object.name, "\n")
                plan.results <- get(envir = .frame0, results.object.name)
            }
            else {
                cat("Beginning run to compute", results.object.name,
                  "\n")
                plan.results <- multiple.SimRMD(RMD.pv, the.plan,
                  number.sim = number.sim, plot.stuff = plot.stuff,
                  print.stuff = print.stuff, observation.time.range = observation.time.range)
                attr(plan.results, "RMD.plan") <- the.plan
                attr(plan.results, "RMD.pv") <- RMD.pv
                attr(plan.results, "observation.time.range") <- observation.time.range
                oldClass(plan.results) <- c("RMD.simulation.results",
                  "list")
                MysetOldClass(attr(plan.results, "class"))
                cat("Saving simulations results", results.object.name,
                  "\n")
                assign(envir = .frame0, inherits = !TRUE,results.object.name, plan.results)
            }
            results.matrix[, j] <- SummarizeMultiple.SimRMDOverTime
            SimulatedQuantileSummary(plan.results, p = p, plot.stuff = plot.stuff,
                time.vec = time.vec)$standard.deviations
        }
        plot.paper(range(time.vec), range(results.matrix), grids = F,
            xlab = "Time", ylab = ylab)
        title(paste("Standard Deviations of ML Estimates \nof the",
            p, "Degradation Quantile for Plan", the.plan.string))
        for (j in 1:length(EvaluateEndTimes)) {
            lines(time.vec, results.matrix[, j], type = report.type,
                lty = lty)
            text(x.loc(0.94), results.matrix[nrow(results.matrix),
                j], paste("After", as.character(EvaluateEndTimes[j])))
        }
    }
}
