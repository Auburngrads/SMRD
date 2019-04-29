plot.alt.test.plan <-
function (x, ALT.plan.values, use.conditions = NULL,
    quant.lines = c(0.1, 0.5, 0.9), plot.title.string = "", digits = GetSMRDDefault("SMRD.DigitsPrinted"),
    ...)
{
    if (is.null(use.conditions)) {
        if (is.null(ALT.plan.values$use.conditions))
            stop("Use conditions have not been specified.\n")
        use.conditions <- ALT.plan.values$use.conditions
    }
    the.levels <- signif(x[, attr(x,
        "accelvar.names")], digits = 4)
    if (length(unique(x$censor.times)) > 1) {
        censor.string <- paste("\n Censor times=", paste(x$censor.times,
            collapse = ","))
}   else {censor.string <- paste("\n Censor time=", paste(unique(x$censor.times)))
}
    plan.string <- paste("Levels = ", paste(the.levels, collapse = ","),
        ", n=", paste(x$number.units, collapse = ","),
        censor.string, ", parameters= ", paste(signif(ALT.plan.values$theta.vec,
            digits = digits), collapse = ","), sep = "")
    plan.name.string <- attr(x, "string.name")
    if (is.null(plan.name.string))
        plan.name.string <- deparse(substitute(x))
    plan.values.name.string <- attr(ALT.plan.values, "string.name")
    if (is.null(plan.values.name.string))
        plan.values.name.string <- deparse(substitute(ALT.plan.values))
    inputs.title <- paste("Accelerated Test Plan", plot.title.string,
        "\n", plan.name.string, plan.values.name.string)
    accel.variable <- attr(x, "accelvar.names")
    if (length(accel.variable) > 1) {
        warning("plot.x cannot plot mul accelerating variables test plans")
        return(NULL)
    }
    the.xmat <- as.data.frame(as.matrix(x[, accel.variable]))
    names(the.xmat) <- ALT.plan.values$accelvar.units
    theResponse <- as.matrix(rep(1, nrow(the.xmat)))
    time.units = get.time.units(ALT.plan.values)
    colnames(theResponse)<-as.character(time.units)
    dummy.data.ld <- make.frame.ld(y = theResponse, time.units = time.units,
        xlabel = ALT.plan.values$accelvar.units, the.xmat = the.xmat,
        data.title = paste(inputs.title, "\n", plan.string))
    explan.var <- 1:ncol(xmat(dummy.data.ld))
    relationship <- ALT.plan.values$relationship
    names(relationship) <- names(the.xmat)
    assign(envir = .frame0,  inherits = TRUE,"relationship.vector", relationship)
    formula <- get.default.formula(the.xmat, relationship)
    Terms <- terms(formula)
    dummy.groupm.out <- list(relationship = relationship, theta.hat = ALT.plan.values$theta.vec,
        distribution = ALT.plan.values$distribution, group.var = explan.var,
        focus.variable = names(the.xmat), data.ld = dummy.data.ld,
        terms = Terms)
    basic.plot.alt(groupm.out = dummy.groupm.out, data.ld = dummy.data.ld,
        density.at = c(use.conditions, x[, accel.variable]),
        censor.time = x$censor.times, quant.lines = quant.lines,
        include.data = F, ...)
    invisible()
}
