SummariseRMDSimulationResults <-
function (p = 0.1, time.vec = 1:20, results.vec) 
{
    if (missing(results.vec)) {
        obj.sum.out <- wqm.objects.summary()
        the.ones <- obj.sum.out[, "data.class"] == "RMD.simulation.results"
        results.vec <- dimnames(obj.sum.out)[[1]][the.ones]
        print(results.vec)
    }
    results.matrix <- matrix(NA, ncol = length(results.vec), 
        nrow = length(time.vec))
    plan.string <- rep(NA, length = length(results.vec))
    for (i in 1:length(results.vec)) {
        if (is.character(results.vec[i])) 
            the.results <- get(envir = .frame0, results.vec[i])
        else stop("results.vec should be a character string")
        plan.string[i] <- getPlanString(attr(the.results, "RMD.plan"))
        quantile.summary <- SimulatedQuantileSummary(the.results, 
            p = p, time.vec = time.vec)
        cat("Doing", results.vec[i], plan.string[i], "\n")
        results.matrix[, i] <- quantile.summary$standard.deviations
    }
    plot.paper(range(time.vec), range(results.matrix), grids = F, 
        xlab = "Time", ylab = "Standard Deviations")
    title(paste("Standard Deviations of ML Estimates \nof the", 
        p, "Degradation Quantile"))
    end.point <- length(time.vec)
    for (i in 1:ncol(results.matrix)) {
        lines(time.vec, results.matrix[, i], col = i, lty = i)
        text(x.loc(0.9), results.matrix[end.point, i], plan.string[i])
    }
}
