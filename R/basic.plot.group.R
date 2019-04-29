basic.plot.group <-
function (groupm.list, data.ld, ylim = c(NA, NA), my.title = NULL, 
    title.option = title.option, censor.time = NULL, density.scale = NULL, 
    quant.lines = c(0.1, 0.5, 0.9), cex = 1, xlab = NULL, ylab = get.time.units(data.ld), 
    lwd = 1, grids = F, response.on.yaxis = T, add = F, include.data = T, 
    plot.quant.labels = T, hw.xaxis = NULL, hw.yaxis = NULL, 
    ...) 
{
    `if`(is.onlist("life.data", oldClass(groupm.list[[1]])),
         groupm.out <- groupm.list,
         groupm.out <- groupm.list)
    
    theta.hat <- groupm.out$theta.hat
    distribution <- groupm.out$distribution
    group.var <- groupm.out$group.var
    if (is.null(xlab)) 
        xlab <- get.xlabel(data.ld)[group.var]
    if (is.null(ylab)) 
        ylab <- get.time.units(data.ld)
    logdist <- is.logdist(distribution)
    xlim <- c(0, 1)
    if (is.null(my.title)) {
        my.title <- groupm.out$title
        if (is.null(my.title)) 
            my.title <- get.data.title(data.ld)
    }
    if (is.logdist(distribution)) 
        y.axis <- "log"
    else y.axis <- "linear"
    ylim.data <- NULL
    unique.group <- get.x.markers(data.ld, group.var = group.var, 
        do.order = F)
    yquant.mat <- matrix(NA, ncol = length(quant.lines), nrow = length(unique.group))
    the.x.values <- xmat(data.ld)[, group.var]
    unique.group.names <- sort(unique(the.x.values))
    density.at <- (1:length(unique.group.names) - 0.5)/(length(unique.group.names))
    new.data <- as.data.frame(unique.group.names)
    names(new.data) <- colnames(xmat(groupm.out$data.ld))[group.var]
    get.single.dist.out <- get.single.dist(groupm.out, new.data = new.data[1, 
        , drop = F])
    sigma <- get.single.dist.out$thetavec["Scale"]
    for (i in 1:length(quant.lines)) {
        yquant.mat[, i] <- quantiles.versus.x.groupm.out(groupm.out, 
            new.data = new.data, prob = quant.lines[i])
    }
    quant.lines.list <- list(quant.lines.levels = quant.lines, 
        quant.lines.y = yquant.mat, quant.lines.x = density.at)
    ylim.data.quant <- range(ylim.data, yquant.mat)
    sigmamax <- sigma
    density.anchor.list <- list()
    density.functions.list <- list()
    for (i in 1:length(density.at)) {
        mu <- get.single.dist(groupm.out, new.data = new.data[i, 
            , drop = F])$thetavec["Location"]
        density.functions.list[[i]] <- basic.get.density.lines(density.at = density.at[i], 
            mu = mu, sigma = sigma, distribution = distribution, 
            scale.factor = sigmamax * 0.1, x.axis = "linear")
        density.anchor.list[[i]] <- list(density.anchor.x = rep(density.at[i], 
            length = length(yquant.mat[i, ])), density.anchor.y = yquant.mat[i, 
            ])
        ylim.data.quant <- range(ylim.data.quant, density.functions.list[[i]]$yden)
    }
    character.label.list <- list(at = density.at, labels = as.character(unique.group.names))
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- ylim.data.quant[yrna]
    if (include.data && !is.null(data.ld) &&is.onlist("life.data", 
        oldClass(data.ld))) {
        x.data <- density.at[match(the.x.values, unique.group.names)]
        y.data <-Response(data.ld)
        the.censor.codes <- censor.codes(data.ld)
    }
    else {
        x.data <- NULL
        y.data <- NULL
        the.censor.codes <- NULL
    }
    at.model.plot(x.axis = "linear", y.axis = y.axis, ylim = ylim, 
        xlim = xlim, my.title = my.title, title.option = title.option, 
        censor.time = censor.time, density.scale = density.scale, 
        quant.lines.list = quant.lines.list, density.functions.list = density.functions.list, 
        density.anchor.list = density.anchor.list, character.label.list = character.label.list, 
        cex = cex, xlab = xlab, ylab = ylab, lwd = lwd, grids = grids, 
        response.on.yaxis = response.on.yaxis, x.data = x.data, 
        y.data = y.data, the.censor.codes = the.censor.codes, 
        plot.quant.labels = T, hw.xaxis = hw.xaxis, hw.yaxis = hw.yaxis)
    invisible()
}
