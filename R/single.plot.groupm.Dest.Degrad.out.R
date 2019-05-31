single.plot.groupm.Dest.Degrad.out <-
function (groupm.Dest.Degrad.out,
          FailLevel, 
          quant.lines.levels = c(0.1, 0.5, 0.9), 
          y.axis, 
          density.at, 
          ylim = c(NA,NA), 
          xlim = c(NA,NA), 
          my.title = NULL, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          censor.time = NULL,
          cex = 1, 
          xlab = NULL, 
          ylab = NULL, 
          lwd = 2, 
          grids = F, 
          add = F,
          response.on.yaxis = T,
          the.censor.codes = NULL,
          plot.quant.labels = T,
          nxpoints = 200,...) 
{
    func.call <- match.call()
    transformation.time <- groupm.Dest.Degrad.out$model$transformation.time
    transformation.response <- groupm.Dest.Degrad.out$model$transformation.response
    transformation.x <- fix.inverse.relationship(groupm.Dest.Degrad.out$model$transformation.x)
    if (length(transformation.x) > 1) 
        stop("transformation.x has more than one component")
    x.axis <- transformation.x
    y.axis <- "log"
    the.xmat <- xmat(groupm.Dest.Degrad.out$data.ld)[, groupm.Dest.Degrad.out$group.var]
    distribution <- groupm.Dest.Degrad.out$model$distribution
    logdist <- is.logdist(distribution)
    if (is.null(my.title)) 
        my.title <- paste("Model plot for ", get.data.title(groupm.Dest.Degrad.out$data.ld), 
            "\n", groupm.Dest.Degrad.out$relationship, paste(", Dist:", 
                groupm.Dest.Degrad.out$model$distribution, sep = ""), 
            "\nFailure-time distribution for degradation failure level of ", 
            FailLevel, " ", get.response.units(groupm.Dest.Degrad.out$data.ld), 
            sep = "")
    theta.hat <- groupm.Dest.Degrad.out$origparam
    density.functions.list <- NULL
    if (is.null(density.at)) 
        density.at <- attr(groupm.Dest.Degrad.out, "sub.model")$x.value
    density.at <- as.matrix(density.at)
    trans.x.density.at <- density.at
    if (is.R()) {
        ylim.data <- NULL
        if (!is.null(censor.time) && !is.na(censor.time)) 
            ylim.data <- pp.data(range(censor.time), logdist)
    }
    else {
        ylim.data <- pp.data(range(strip.na(censor.time)), 
            logdist)
        if (any(is.na(ylim.data))) 
            ylim.data <- NULL
    }
    ylim.data.density <- NULL
    if (map.SMRDDebugLevel() >= 4) {
        cat("density.at in single.plot.groupm.Dest.Degrad.out\n")
        print(density.at)
    }
    if (!is.character(density.at)) {
        trans.x.density.at[, 1] <- f.relationship(density.at[, 
            1], transformation.x)
        if (!any(is.na(density.at))) {
            beta2.names <- paste("beta", seq(2, length(transformation.x) + 
                1), sep = "")
            beta2.vec <- theta.hat[beta2.names]
            beta.x <- as.matrix(trans.x.density.at) %*% as.matrix(theta.hat[beta2.names], 
                ncol = 1)
            xfactor <- exp(-beta.x)/abs(theta.hat["beta1"])
            mu.density <- (theta.hat["beta0"] - f.relationship(FailLevel, 
                transformation.response)) * xfactor
            sigma.density <- theta.hat[length(theta.hat)] * xfactor
            sigmamax <- max(sigma.density)
            model.table <- cbind(density.at, mu = mu.density, 
                sig = sigma.density, ratio = mu.density/sigma.density)
            if (map.SMRDDebugLevel() >= 2) 
                print(model.table)
            density.functions.list <- list()
            for (i in 1:nrow(trans.x.density.at)) {
                density.functions.list[[i]] <- basic.get.exp.trans.density.lines(trans.x.density.at = trans.x.density.at[i, 
                  , drop = F], focus.variable = 1, theta.hat = theta.hat, 
                  distribution = distribution, scale.factor = 1, 
                  transformation.x = transformation.x, transformation.time = transformation.time, 
                  transformation.response = transformation.response, 
                  FailLevel = FailLevel)
                ylim.data.density <- range(ylim.data.density, 
                  density.functions.list[[i]]$yden)
            }
        }
    }
    else density.at <- NULL
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(density.at, the.xmat)[xrna]
    expand.factor <- 1.000001
    xlim <- c(xlim[1]/expand.factor, xlim[2] * expand.factor)
    quant.lines.x <- seq(xlim[1], xlim[2], length = nxpoints)
    quant.lines.y <- matrix(NA, ncol = length(quant.lines.levels), 
        nrow = length(quant.lines.x))
    igood <- rep(F, length(quant.lines.levels))
    the.full.conditions <- rep(NA, length = length(transformation.x))
    untran.xvalues <- matrix(as.matrix(the.full.conditions), 
        nrow = nxpoints, ncol = length(transformation.x), byrow = T)
    untran.xvalues[, 1] <- quant.lines.x
    x.tran <- untran.xvalues
    x.tran[, 1] <- f.relationship(untran.xvalues[, 1], transformation.x)
    for (i in 1:length(quant.lines.levels)) {
        the.quant.lines.y <- ADDT.life.quantile(quant.lines.levels[i], 
            distribution, f.relationship(FailLevel, transformation.response), 
            x.tran, theta.hat)
        if (all(the.quant.lines.y > 0)) {
            igood[i] <- T
            quant.lines.y[, i] <- f.relationshipinv(the.quant.lines.y, 
                transformation.time)
        }
    }
    quant.lines.y <- quant.lines.y[, igood, drop = F]
    quant.lines.levels <- quant.lines.levels[igood]
    quant.lines.list <- list(quant.lines.levels = quant.lines.levels, 
        quant.lines.y = quant.lines.y, quant.lines.x = quant.lines.x)
    ylim.data.quant <- range(ylim.data.density, ylim.data, 
        quant.lines.y)
    if (is.null(xlab)) {
        x.axis.name <- fix.axis.name(x.axis)
        if (generic.relationship.name(x.axis) == "Box-Cox") {
            the.power <- attr(x.axis, "the.power")
            x.axis.name <- paste(x.axis.name, "(", the.power, 
                ")", sep = "")
        }
        if (length(groupm.Dest.Degrad.out$group.var) > 1) 
            stop(paste("length(group.var)=", paste(groupm.Dest.Degrad.out$group.var, 
                collapse = ",")))
        xunits <- groupm.Dest.Degrad.out$x.name
        if (is.null(xunits)) {
            xunits <- dimnames(the.xmat)[[2]]
            warning("need to define groupm.Dest.Degrad.out$x.name")
        }
        xlab <- paste(xunits, "on", x.axis.name, "Scale")
    }
    if (is.null(ylab)) {
        ylab <- get.time.units(groupm.Dest.Degrad.out$data.ld)
    }
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(ylim.data.quant)[yrna]
    at.model.plot(x.axis = x.axis, y.axis = y.axis, ylim = ylim, 
        xlim = xlim, my.title = my.title, title.option = title.option, 
        censor.time = censor.time, density.functions.list = density.functions.list, 
        quant.lines.list = quant.lines.list, cex = cex, xlab = xlab, 
        ylab = ylab, lwd = lwd, grids = grids, response.on.yaxis = response.on.yaxis, 
        plot.quant.labels = plot.quant.labels, ...)
    text(x.loc(0.03), y.loc(0.07), paste("Probability spike at time zero =", 
        format(density.functions.list[[1]]$spike.at.zero)), adj = 0)
    invisible()
}
