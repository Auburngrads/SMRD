basic.plot.alt <-
function (groupm.out, data.ld, ylim = c(NA, NA), xlim = c(NA,
    NA), my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), units = NULL,
    yunits = NULL, censor.time = NULL, density.scale = NULL,
    density.at = "Automatic", add.density.at = NULL, quant.lines = c(0.1,
        0.5, 0.9), cex = 1, nxpoints = 50, xlab = NULL, ylab = NULL,
    lwd = 2, grids = F, add = F, response.on.yaxis = T, include.data = T,
    plot.quant.labels = T, range.of.focus = NULL, hw.xaxis = NULL,
    hw.yaxis = NULL, ...)
{
    relationship <- groupm.out$relationship
    theta.hat <- groupm.out$theta.hat
    distribution <- groupm.out$distribution
    explan.var <- groupm.out$group.var
    if (!is.null(density.at) && is.character(density.at) && density.at ==
        "Automatic")
        density.at <- unique(c(xmat(data.ld)[, explan.var], add.density.at))
    if (is.character(density.at) && density.at == "None")
        density.at <- NULL
    xunits <- get.xlabel(data.ld)[explan.var]
    yunits <- get.time.units(data.ld)
    number.param <- length(theta.hat)
    if (is.null(relationship))
        stop("No relationship in the results structure")
    x.axis <- fix.inverse.relationship(relationship)
    txlim <- range(xmat(data.ld)[, explan.var], density.at,
        strip.na(range.of.focus))
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- txlim[xrna]
    if (is.null(my.title)) {
        my.title <- groupm.out$title
        if (is.null(my.title))
            my.title <- get.data.title(data.ld)
    }
    if (is.logdist(distribution))
        y.axis <- "log"
    else y.axis <- "linear"
    expand.factor <- 1.000001
    xlim <- c(xlim[1]/expand.factor, xlim[2] * expand.factor)
    if (is.logdist(distribution))
        xvec <- logseq(txlim[1], txlim[2], length = nxpoints)
    else xvec <- seq(txlim[1], txlim[2], length = nxpoints)
    if (include.data)
        ylim.data <- range(Response(data.ld), strip.na(censor.time))
    if (!include.data || any(is.na(ylim.data)))
        ylim.data <- NULL
    new.data <- as.data.frame(xvec)
    if (is.null(groupm.out$focus.variable))
        stop("Focus variable not identified")
    names(new.data) <- groupm.out$focus.variable
    get.single.dist.vector.x.out <- get.single.dist.vector.x(groupm.out,
        new.data = new.data, do.vcv = F)
    mu <- get.single.dist.vector.x.out$theta.hat[, "Location"]
    sigma <- get.single.dist.vector.x.out$theta.hat[, "Scale"]
    sigmamax <- max(sigma)
    yquant.mat <- matrix(NA, ncol = length(quant.lines), nrow = length(xvec))
    for (i in 1:length(quant.lines)) {
        yquant.mat[, i] <- mu + quant(quant.lines[i], distribution) *
            sigma
    }
    if (is.logdist(distribution)) {
        yquant.mat <- exp(yquant.mat)
    }
    quant.lines.list <- list(quant.lines.levels = quant.lines,
        quant.lines.y = yquant.mat, quant.lines.x = xvec)
    ylim.data.quant <- range(ylim.data, yquant.mat)
    density.functions.list <- list()
    if (!is.null(density.at)) {
        for (i in 1:length(density.at)) {
            frame.density.at <- as.data.frame(density.at[i])
            names(frame.density.at) <- groupm.out$focus.variable
            mu <- get.single.dist(groupm.out, new.data = frame.density.at[1,
                , drop = F])$thetavec["Location"]
            density.functions.list[[i]] <- basic.get.density.lines(density.at = density.at[i],
                mu = mu, sigma = sigma, distribution = distribution,
                scale.factor = sigmamax * 0.1, x.axis = x.axis)
            ylim.data.quant <- range(ylim.data.quant, density.functions.list[[i]]$yden)
        }
    }
    if (is.null(xlab)) {
        if (generic.relationship.name(relationship) == "Box-Cox") {
            the.power <- attr(relationship, "the.power")
            x.axis.name <- paste(relationship, "(", the.power,
                ")", sep = "")
        }
        else x.axis.name <- relationship
        xlab <- paste(xunits, "on", x.axis.name, "Scale")
    }
    if (is.null(ylab)) {
        ylab <- yunits
    }
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- ylim.data.quant[yrna]
    if (include.data && !is.null(data.ld) &&is.onlist("life.data",
        oldClass(data.ld))) {
        x.data <- xmat(data.ld)[, explan.var]
        y.data <-Response(data.ld)
        the.censor.codes <- censor.codes(data.ld)
    }
    else {
        x.data <- NULL
        y.data <- NULL
        the.censor.codes <- NULL
    }
    at.model.plot(x.axis = x.axis, y.axis = y.axis, ylim = ylim,
        xlim = xlim, my.title = my.title, title.option = title.option,
        censor.time = censor.time, density.scale = density.scale,
        quant.lines.list = quant.lines.list, density.functions.list = density.functions.list,
        cex = cex, xlab = xlab, ylab = ylab, lwd = lwd, grids = grids,
        response.on.yaxis = response.on.yaxis, x.data = x.data,
        y.data = y.data, the.censor.codes = the.censor.codes,
        plot.quant.labels = T, hw.xaxis = hw.xaxis, hw.yaxis = hw.yaxis)
    invisible()
}
