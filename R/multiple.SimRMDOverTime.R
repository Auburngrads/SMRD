multiple.SimRMDOverTime <-
function (RMD.pv, RMD.plan.vec, EvaluateEndTimes, begin.time = 1,
    number.sim = 200, plot.stuff = F, print.stuff = F, time.vec = 1:20,
    report.type = "b")
{
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
            cat("Beginning run", results.object.name, "\n")
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
            assign(envir = .frame0,  inherits = TRUE,results.object.name, plan.results)
        }
    }
}
