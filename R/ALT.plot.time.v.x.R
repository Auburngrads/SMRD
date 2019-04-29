ALT.plot.time.v.x <-
function (results.object, x.of.interest = NULL, plan.values.string = NULL,
    plan.string = NULL, quantile.list = c(0.5), ylim = c(NA,
        NA), xlim = c(NA, NA), xlab = NULL, ylab = NULL, my.title = NULL,
    title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F, numplotsim = 50, nxpoints = 60,
    response.on.yaxis = T, cex = 1, focus.variable = 1)
{
  AT.levels <-
    function (ADDT.test.plan)
    {
      levels.columns <- attr(ADDT.test.plan, "levels.columns")
      levels <- ADDT.test.plan[, levels.columns, drop = F]
      col.names <- dimnames(ADDT.test.plan)[[2]]
      names(col.names) <- col.names
      dimnames(levels) <- list(as.character(1:nrow(levels)), col.names[levels.columns])
      oldClass(levels) <- "data.frame"
      return(levels)
    }
    number.sim <- nrow(results.object)
    use.conditions <- x.of.interest
    if (is.null(use.conditions)) {
        use.conditions <- attr(results.object, "use.conditions")
        if (is.null(use.conditions))
            use.conditions <- attr(results.object, "plan.values")$use.conditions
        if (is.null(use.conditions))
            stop("Use conditions not specified")
    }
    if (is.character(use.conditions))
        use.conditions <- string.to.frame(use.conditions)
    else if (is.numeric(use.conditions) && !is.data.frame(use.conditions))
        use.conditions <- as.data.frame(matrix(use.conditions,
            ncol = length(use.conditions)))
    character.use.conditions <- as.character(use.conditions)
    character.use.conditions[focus.variable] <- ""
    ALT.plan.values <- attr(results.object, "plan.values")
    ALT.test.plan <- attr(results.object, "plan")
    par(err = -1)
    if (is.null(plan.string))
        plan.string <- attr(results.object, "plan.string")
    if (is.null(plan.values.string))
        plan.values.string <- attr(results.object, "plan.values.string")
    if (is.null(xlab))
        xlab <- ALT.plan.values$accelvar.units[focus.variable]
    if (is.null(ylab))
        ylab <- get.time.units(ALT.plan.values)
    distribution <- ALT.plan.values$distribution
    orig.relationship <- ALT.plan.values$relationship
    model.string <- paste("x:", paste(ALT.plan.values$relationship,
        character.use.conditions, collapse = ","), ", Dist:",
        distribution, sep = "")
    if (is.null(my.title))
        my.title <- paste("Accelerated life test simulation based on\n",
            plan.string, plan.values.string, "\nFailure time",
            quantile.list[1], "quantile vs", xlab, "\n", model.string)
    relationship <- fix.inverse.relationship(orig.relationship)
    slope.name <- attr(orig.relationship, "slope.name")
    y.axis <- "log"
    the.xmat <- AT.levels(ALT.test.plan)
    x.derived.range <- range(the.xmat[, focus.variable], as.matrix(use.conditions)[,
        focus.variable], xlim, na.rm = T)
    tx.derived.range <- f.relationship(x.derived.range, subscript.relationship(relationship,
        focus.variable))
    txvec <- seq(tx.derived.range[1], tx.derived.range[2], length = nxpoints)
    focus.untran.x <- f.relationshipinv(txvec, subscript.relationship(relationship,
        focus.variable))
    untran.xvalues <- matrix(as.matrix(use.conditions), nrow = length(focus.untran.x),
        ncol = ncol(as.matrix(use.conditions)), byrow = T)
    untran.xvalues[, focus.variable] <- focus.untran.x
    dimnames(untran.xvalues) <- list(dimnames(untran.xvalues)[[1]],
        names(the.xmat))
    x.tran <- untran.xvalues
    for (i in 1:ncol(untran.xvalues)) {
        x.tran[, i] <- f.relationship(untran.xvalues[, i], subscript.relationship(orig.relationship,
            i))
    }
    true.results <- ALT.life.quantile(theta.hat = ALT.plan.values$theta.vec,
        p = quantile.list[1], distribution = ALT.plan.values$distribution,
        xuse.tran = x.tran)
    response.mat <- apply(matrix(results.object[, 1:length(ALT.plan.values$theta.vec),
        drop = F], nrow = nrow(results.object)), 1, ALT.life.quantile,
        p = quantile.list[1], distribution = ALT.plan.values$distribution,
        xuse.tran = x.tran)
    numplotsim <- min(ncol(response.mat), numplotsim)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- x.derived.range[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(response.mat)[yrna]
    at.model.plot(x.axis = subscript.relationship(relationship,
        focus.variable), y.axis = y.axis, ylim = ylim,
        xlim = xlim, my.title = my.title, title.option = title.option,
        cex = cex, xlab = xlab, ylab = ylab, grids = grids, response.on.yaxis = response.on.yaxis)
    take.out <- c(1, 2, length(txvec) - 1, length(txvec))
    lines(txvec, logb(true.results), col = 1, lwd = 4, lty = 1)
    matlines(txvec[-take.out], logb(response.mat[-take.out, 1:numplotsim]),
        col = 1, lty = 2)
    invisible()
}
