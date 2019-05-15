#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param stresses 
#' @param group.var 
#' @param contour.indicators 
#' @param xlim 
#' @param ylim 
#' @param loc.or.quant 
#' @param the.quantile 
#' @param rel.or.conf 
#' @param my.title 
#' @param cex 
#' @param title.option 
#' @param do.legend 
#' @param stresses.limit 
#' @param size 
#' @param the.factor 
#' @param show.mle 
#' @param original.par 
#' @param col.fhat.vec 
#' @param lty 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Snubber.ld <- frame.to.ld(snubber, 
#'                           response.column = "cycles", 
#'                           censor.column = "event",
#'                           time.units = "Cycles",
#'                           case.weight.column = "count",
#'                           x.columns = "design")
#' ```
#' 
#' event.plot(Snubber.ld)
#' summary(Snubber.ld)
#' 
#' Snubber.groupi.nor.out <-  groupi.mleprobplot(Snubber.ld,"normal")
#' 
#' tmpxx <- groupi.contour(Snubber.ld,"Weibull",the.quantile=.1)
#' tmpxx <- groupi.contour(Snubber.ld,"lognormal",the.quantile=.1)
#' tmpxx <- groupi.contour(Snubber.ld,"lognormal")
#' tmpxx <- groupi.contour(Snubber.ld,"normal")
#' tmpxx <- groupi.contour(Snubber.ld,"normal",the.quantile=.1)
#' 
#' }
groupi.contour <-
function (data.ld, distribution, stresses = get.x.markers(data.ld,
    group.var = group.var), group.var = 1, contour.indicators = NULL,
    xlim = c(NA, NA), ylim = c(NA, NA), loc.or.quant = "Quantile",
    the.quantile = 0.1, rel.or.conf = "Joint confidence region",
    my.title = NULL, cex = 1.5, title.option = GetSMRDDefault("SMRD.TitleOption"), do.legend = "On plot",
    stresses.limit = 15, size = 50, the.factor = 3.5, show.mle = F,
    original.par = T, col.fhat.vec = 1:length(stresses), lty = (1:(length(stresses) +
        1))[-2], ...)
{
    dist.name <- distribution.name(distribution)
    number.group.var <- length(group.var)
    stress.names <- get.x.markers(data.ld, group.var = group.var,
        long = T)
    if (!is.null(my.title) && my.title == "")
        top.mar <- 4
    else top.mar <- 7
    if (original.par) {
        old.par <- par(mar = c(6, 8, top.mar, 6) + 0.1, cex = 1.2)
        on.exit(par(old.par))
    }
    if (length(stresses) > stresses.limit) {
        warning(paste("\n\nThere are", length(stresses),
            "combinations of explanatory variable levels;\nonly the first",
            stresses.limit, "will be used."))
        length(stresses) <- stresses.limit
    }
    if (loc.or.quant == "Location") {
        log.quantile <- T
        xname <- "mu"
        zquant <- 0
        the.quantile <- wqmf.phibf(zquant, distribution)
}   else {
        if (is.logdist(distribution))
            log.quantile <- F
        else log.quantile <- T
        xname <- paste(the.quantile, "Quantile", get.time.units(data.ld), sep = "~")
        zquant <- quant(the.quantile, distribution)
    }
    ci.quant.list <- list()
    ci.spread.list <- list()
    empirical.xlim <- NULL
    empirical.ylim <- NULL
    data.list <- list()
    grid.list <- list()
    gmle.out.list <- list()
    for (i in 1:length(stresses)) {
        data.list.ld <- multiple.get.data.subset(data.ld, stresses[i],
            columns = group.var)
        the.right.stuff <- .right.stuff(data.list.ld)
        data.list[[stress.names[i]]] <- data.list.ld
        if (!good.data(data.list.ld)) {
            cat(paste("Skipping", stress.names[i], "because too few failures\n"))
            next
        }
        mlest.out <- mlest(data.list.ld, distribution)
        theta.hat <- mlest.out$theta.hat
        vcv.matrix <- mlest.out$vcv.matrix
        gmle.out.list[[stress.names[i]]] <- ls2.mle(data.list.ld,
            distribution = distribution, theta.start = theta.hat)
        if (loc.or.quant == "Location") {
            ci.quant.list[[stress.names[i]]] <- c(theta.hat[1] -
                the.factor * sqrt(vcv.matrix[1, 1]), theta.hat[1] +
                the.factor * sqrt(vcv.matrix[1, 1]))
}       else {
            quanthat <- theta.hat[1] + zquant * theta.hat[2]
            sequant <- sqrt(vcv.matrix[1, 1] + (zquant^2) * vcv.matrix[2,
                2] + 2 * zquant * vcv.matrix[1, 2])
            ci.quant.list[[stress.names[i]]] <- c(quanthat -
                the.factor * sequant, quanthat + the.factor *
                sequant)
            if (is.logdist(distribution)) {
                ci.quant.list[[stress.names[i]]] <- exp(ci.quant.list[[stress.names[i]]])
            }
        }
        ci.spread <- c(theta.hat[2]/exp((the.factor * sqrt(vcv.matrix[2,
            2]))/theta.hat[2]), theta.hat[2] * exp((the.factor *
            sqrt(vcv.matrix[2, 2]))/theta.hat[2]))
        if (generic.distribution(distribution) == "weibull") {
            spread.name <- "beta"
            spread.hat <- 1/theta.hat[2]
            ci.spread <- sort(1/ci.spread)
}       else {
            spread.name <- "sigma"
            spread.hat <- theta.hat[2]
        }
        gmle.out.list[[stress.names[i]]]$theta.hat <- theta.hat
        ci.spread.list[[stress.names[i]]] <- ci.spread
        empirical.xlim <- range(empirical.xlim, ci.quant.list[[stress.names[i]]])
        empirical.ylim <- range(empirical.ylim, ci.spread.list[[stress.names[i]]])
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- empirical.xlim[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- empirical.ylim[yrna]
    oldClass(data.list) <- "multiple.life.data"
    for (i in 1:length(gmle.out.list)) {
        if (i == 1)
            my.add <- F
        else my.add <- T
        ci.quant <- ci.quant.list[[i]]
        ci.spread <- ci.spread.list[[i]]
        likelihood.grid.out <- likelihood.grid(gmle.out.list[[i]],
            the.quantile = the.quantile, log.quantile = log.quantile,
            range.list = list(ci.quant, ci.spread), relative = T,
            size = size, xname = xname, yname = spread.name)
        grid.list[[stress.names[i]]] <- likelihood.grid.out
        if (rel.or.conf == "Joint confidence region") {
            if (is.null(my.title))
                my.title <- paste(get.data.title(data.ld), "\nIndividual",
                  dist.name, "Distribution Joint Confidence Regions")
            if (is.null(contour.indicators)) {
                contour.indicators <- c(50, 95)
            }
            conf.contour(likelihood.grid.out, xlim = xlim,
                ylim = ylim, profile.title = "", transformationx = "linear",
                variable.namey = spread.name, transformationy = "linear",
                levels = contour.indicators, add = my.add, original.par = F,
                lty = lty[i], col = col.fhat.vec[i])
}       else {
            if (is.null(my.title))
                my.title <- paste(get.data.title(data.ld), "\nIndividual",
                  dist.name, "Distribution Relative Likelihoods")
            if (is.null(contour.indicators)) {
                contour.indicators <- c(0.1, 0.2, 0.3, 0.5, 0.9)
            }
            profile.contour(likelihood.grid.out, xlim = xlim,
                ylim = ylim, profile.title = "", transformationx = "linear",
                variable.namey = spread.name, transformationy = "linear",
                levels = contour.indicators, add = my.add, lty = lty[i],
                col = col.fhat.vec[i])
        }
        if (show.mle) {
            theta.hat <- gmle.out.list[[i]]$theta.hat
            arrows(theta.hat[1], theta.hat[2], theta.hat[1],
                y.loc(0), open = T, col = col.fhat.vec[i])
            arrows(theta.hat[1], theta.hat[2], x.loc(0), theta.hat[2],
                open = T, col = col.fhat.vec[i])
        }
    }
    do.list <- names(gmle.out.list)
    if (do.legend == "On plot")
        legend(x.loc(0.003), y.loc(0.996), do.list, cex = 1.1,
            bty = "n", col = col.fhat.vec, lty = lty, y.intersp = 0.675)
    if (do.legend == "New page" || do.legend == "New file") {
        if (do.legend == "New file")
            postscript(file = "Save_legend.ps", horizontal = T)
        plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
            xaxt = "n", yaxt = "n")
        legend(x.loc(0.003), y.loc(0.997), do.list, cex = 1.1,
            bty = "n", col = col.fhat.vec, lty = lty, y.intersp = 0.675)
        if (do.legend == "New file")
            dev.off()
    }
    invisible()
    if (title.option == "full")
        title(my.title, cex = 0.8)
    return(grid.list)
}
