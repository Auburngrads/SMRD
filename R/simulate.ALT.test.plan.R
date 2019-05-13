#' @export
simulate.ALT.test.plan <-
function (object, nsim = 10, seed = NULL, ALT.plan.values, use.conditions, quantile.list = c(0.1,
    0.5, 0.9), ylim = c(NA, NA), xlim = c(NA, NA), my.title = NULL,
    title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F, numplotsim = 50,
    nxpoints = 50, response.on.yaxis = T, show.detail.on = 0,
    cex = 1, focus.variable = 1,...)
{
    if (missing(use.conditions)) {
        if (is.null(ALT.plan.values$use.conditions))
            stop("\n Use conditions have not been specified.\n")
    }
    character.use.conditions <- as.character(use.conditions)
    character.use.conditions[focus.variable] <- ""
    plan.string <- attr(object, "string.name")
    if (is.null(plan.string)) {
        plan.string <- deparse(substitute(object))
        attr(object, "string.name") <- plan.string
    }
    plan.values.string <- attr(ALT.plan.values, "string.name")
    if (is.null(plan.values.string)) {
        plan.values.string <- deparse(substitute(ALT.plan.values))
        attr(ALT.plan.values, "string.name") <- plan.values.string
    }
    xlab <- ALT.plan.values$accelvar.units[focus.variable]
    if (is.null(my.title))
        my.title <- paste("Accelerated life test simulation based on\n",
            plan.string, plan.values.string, "\n")
    model.string <- paste("x:", paste(ALT.plan.values$relationship,
        character.use.conditions, collapse = ","), ", Dist:",
        ALT.plan.values$distribution, sep = "")
    plot.spec.title <- paste(my.title, model.string, "\nFailure time",
        quantile.list[1], "quantile vs", xlab)
    sim.data.title <- paste("Simulated data from", plan.string,
        plan.values.string)
    ALTsim.full.out <- ALTsim(ALT.test.plan = object,
        ALT.plan.values = ALT.plan.values, number.sim = nsim,
        show.detail.on = show.detail.on, xlim = xlim, ylim = ylim,
        sim.data.title = sim.data.title, use.conditions = use.conditions)
    model.string <- paste("x:", paste(ALT.plan.values$relationship,
        collapse = ","), ", Dist:", ALT.plan.values$distribution,
        sep = "")
    attr(ALTsim.full.out, "title") <- paste(my.title, model.string)
    attr(ALTsim.full.out, "plan.string") <- plan.string
    attr(ALTsim.full.out, "plan.values.string") <- plan.values.string
    attr(ALTsim.full.out, "Accelerated Life Test Simulation Results") <- "xxxxx"
    attr(ALTsim.full.out, "use.conditions") <- as.data.frame(matrix(use.conditions,
        ncol = length(use.conditions)))
    ALT.plot.time.v.x(ALTsim.full.out, quantile.list = quantile.list,
        my.title = plot.spec.title, title.option = title.option,
        grids = grids, numplotsim = numplotsim, nxpoints = nxpoints,
        response.on.yaxis = response.on.yaxis, cex = cex)
    invisible(ALTsim.full.out)
}
