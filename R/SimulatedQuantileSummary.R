SimulatedQuantileSummary <-
function (sim.results, p, time.vec = sort(unique(unlist(RMD.plan))),
    plot.stuff = T, max.lines.to.plot = 50, xlim = range(time.vec),
    ylim = range(quantile.estimation.results))
{
    RMD.plan <- attr(sim.results, "RMD.plan")
    time.units <- attr(RMD.plan, "time.units")
    if (is.null(time.units)) {
        time.units <- "Time"
        warning("Time units not in results---suggest remake")
    }
    plan.string <- getPlanString(RMD.plan)
    results.matrix <- sim.results$results.matrix
    observation.time.range <- attr(sim.results, "observation.time.range")
    number.sim <- nrow(results.matrix)
    get.quantiles <- function(p, rmd.parameters, time.vec) {
        meanDt <- rmd.parameters["mu.beta0"] + rmd.parameters["mu.beta1"] *
            time.vec
        cov <- rmd.parameters["rho"] * rmd.parameters["sigma.beta0"] *
            rmd.parameters["sigma.beta1"]
        sdDt <- sqrt(rmd.parameters["sigma.beta0"]^2 + time.vec^2 *
            rmd.parameters["sigma.beta1"]^2 + 2 * time.vec *
            cov)
        the.quantiles <- meanDt + qnorm(p) * sdDt
        return(the.quantiles)
    }
    if (is.na(p))
        stop("Must specify specific quanti from le p in SimulatedQuantileSummary")
    quantile.estimation.results <- apply(results.matrix, 1, get.quantiles,
        p = p, time.vec = time.vec)
    true.parameters <- attr(sim.results, "RMD.pv")
    quantile.true.parameters <- get.quantiles(true.parameters,
        p = p, time.vec = time.vec)
    if (plot.stuff) {
        ylab <- paste(p, "Quantile")
        plot.paper(xlim, ylim, grids = F, xlab = time.units,
            ylab = ylab)
        hold.out <- c(1, nrow(quantile.estimation.results))
        my.title <- paste("ML Estimates and the True", p, "Quantile Line\nPlan",
            plan.string, "to time", observation.time.range[2],
            "based on", number.sim, "simulations")
        title(my.title)
        lines.to.plot <- 1:min(max.lines.to.plot, ncol(quantile.estimation.results))
        matlines(time.vec[-hold.out], quantile.estimation.results[-hold.out,
            lines.to.plot], lty = 1, col = 1)
        lines(time.vec, quantile.true.parameters, lwd = 5, lty = 1,
            col = 2)
    }
    variances <- apply(quantile.estimation.results, 1, var)
    invisible(list(time.vec = time.vec, standard.deviations = sqrt(variances)))
}
