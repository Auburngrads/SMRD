simulate.ADDT.test.plan <-
function (object, nsim = 3, seed = NULL, ADDT.plan.values, use.conditions, FailLevel,
    quantile.list = c(0.1, 0.5, 0.9), ylim = c(NA, NA), xlim = c(NA,
        NA), my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"),
    grids = F, numplotsim = 50, nxpoints = 50, response.on.yaxis = T,
    plotem = 2, cex = 1, focus.variable = 1,...)
{
    transformation.time <- ADDT.plan.values$transformation.time
    if (missing(use.conditions)) {
        use.conditions <- ADDT.plan.values$use.condition
        if (is.null(use.conditions))
            stop("Use conditions must be specified in simulation if they were not included in the plan values")
    }
    if (is.character(use.conditions))
        use.conditions <- string.to.frame(use.conditions)
    if (missing(FailLevel)) {
        FailLevel <- ADDT.plan.values$FailLevel
    }
    plan.string <- attr(object, "string.name")
    if (is.null(plan.string)) {
        plan.string <- deparse(substitute(object))
        attr(object, "string.name") <- plan.string
    }
    plan.values.string <- attr(ADDT.plan.values, "string.name")
    if (is.null(plan.values.string)) {
        plan.values.string <- deparse(substitute(ADDT.plan.values))
        attr(ADDT.plan.values, "string.name") <- plan.values.string
    }
    FailLevelDef <- paste(FailLevel, get.response.units(ADDT.plan.values))
    transformation.response <- ADDT.plan.values$transformation.response
    character.use.condition <- as.character(use.conditions)
    character.use.condition[focus.variable] <- ""
    model.string <- paste("Resp:", transformation.response, ",Time:",
        transformation.time, ",x:", paste(ADDT.plan.values$transformation.x,
            character.use.condition, collapse = ","), ", Dist:",
        ADDT.plan.values$distribution, sep = "")
    xlab <- ADDT.plan.values$accelvar.units[focus.variable]
    my.title <- paste("Accelerated destructive degradation test simulation based on\n",
        plan.string, plan.values.string, "\nFailure time", quantile.list[1],
        "quantile vs", xlab, "for failure definition", FailLevelDef,
        "\n", model.string)
    sim.data.title <- paste("Simulated Data based on", plan.string,
        plan.values.string)
    ADDTsim.full.out <- ADDTsim(ADDT.plan.values = ADDT.plan.values,
        ADDT.test.plan = object, number.sim = nsim,
        plotem = plotem, xlim = xlim, ylim = ylim,
        sim.data.title = sim.data.title)
    attr(ADDTsim.full.out, "title") <- my.title
    attr(ADDTsim.full.out, "plan.string") <- plan.string
    attr(ADDTsim.full.out, "plan.values.string") <- plan.values.string
    attr(ADDTsim.full.out, "Accelerated Life Test Simulation Results") <- paste(attr(ADDTsim.full.out,
        "title"), "\nfrom", plan.string, plan.values.string)
    attr(ADDTsim.full.out, "FailLevel") <- FailLevel
    attr(ADDTsim.full.out, "use.conditions") <- use.conditions
    if (F) {
        if (is.sv3())
            attr(ADDTsim.full.out, "use.conditions") <- as.data.frame(matrix(use.conditions,
                ncol = length(use.conditions)))
        else attr(ADDTsim.full.out, "use.conditions") <- use.conditions
    }
    ADDT.plot.time.v.x(ADDTsim.full.out, quantile.list = quantile.list,
        my.title = my.title, title.option = title.option, grids = grids,
        numplotsim = numplotsim, nxpoints = nxpoints, response.on.yaxis = response.on.yaxis,
        cex = cex)
    invisible(ADDTsim.full.out)
}
