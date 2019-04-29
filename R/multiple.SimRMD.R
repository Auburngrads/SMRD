multiple.SimRMD <-
function (RMD.pv, RMD.plan, number.sim, plot.stuff = F, print.stuff = F,
    observation.time.range = range(unlist(RMD.plan)))
{
    results.matrix <- matrix(NA, ncol = 6, nrow = number.sim)
    dimnames(results.matrix) <- list(rep("", number.sim), names(RMD.pv))
    log.det.vec <- rep(NA, length = number.sim)
    plan.string <- paste(getPlanString(RMD.plan), "End", observation.time.range[2])
    ii <- 0
    for (i in 1:number.sim) {
        results <- try(SimRMD(RMD.pv, RMD.plan, plot.stuff = plot.stuff,
            print.stuff = print.stuff, observation.time.range = observation.time.range))
        if (oldClass(results) == "Error") {
            cat("Error in simulation", i, "\n")
            bad.data.name <- paste("Bad.", i, ".RMD.data.grouped",
                sep = "")
            assign(envir = .frame0,  inherits = TRUE,bad.data.name, get(envir = .frame0,  "RMD.data.grouped"))
       } else {
            ii <- ii + 1
            summary.results <- DegSummary(results)
            results.matrix[ii, ] <- summary.results$parameters
            log.det.vec[ii] <- summary.results$log.det
        }
    }
    results.matrix <- results.matrix[1:ii, , drop = F]
    length(log.det.vec) <- ii
    return.list <- list(results.matrix = results.matrix, log.det.vec = log.det.vec)
    attr(return.list, "RMD.plan") <- RMD.plan
    attr(return.list, "RMD.pv") <- RMD.pv
    attr(return.list, "observation.time.range") <- observation.time.range
    oldClass(return.list) <- c("RMD.simulation.results", "list")
    invisible(return.list)
}
