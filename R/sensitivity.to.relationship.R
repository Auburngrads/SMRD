sensitivity.to.relationship <-
function (groupm.out.list, new.data, which.relationship, the.quantile.vec = 0.1,
    distribution = NULL, power.vec = NULL, my.title = NULL, add = F,
    title.option = GetSMRDDefault("SMRD.TitleOption"), ylim = c(NA, NA), xlim = c(NA,
        NA), xlab = paste(get.x.columns(data.ld)[group.var[which.relationship]],
        "Box-Cox Transformation Power"), conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    ylab = NULL, plotem = F, grids = F, y.axis = NULL, do.legend = "On plot",
    cex = 1.2,debug1= T, do.sensitivity.plot = T, do.scale = T)
{
    data.ld <- groupm.out.list$groupm.out$data.ld
    group.var <- groupm.out.list$groupm.out$group.var
    formula <- as.formula(attr(groupm.out.list$groupm.out$terms,
        "formula"))
    if (is.null(power.vec))
        power.vec <- c(-1, -0.5, 0, 0.5, 1, 1.5, 2)
    power.vec <- sort(power.vec)
    if (is.null(ylab)) {
        if (length(the.quantile.vec) == 1)
            ylab <- paste(the.quantile.vec, "Quantile of", get.time.units(data.ld),
                "Distribution")
        else ylab <- paste("Quantile of", get.time.units(data.ld),
            "distribution")
    }
    relationship <- multiple.generic.relationship.name(groupm.out.list$groupm.out$relationship)
    if (which.relationship < 1 || which.relationship > length(relationship))
        stop(paste("which.relationship", which.relationship,
            "is outside the range of", paste(relationship, collapse = ",")))
    replaced.relationship <- relationship[which.relationship]
    if (!is.onlist(replaced.relationship, c("log", "squareroot",
        "linear", "reciprocal"))) {
        the.relationship.now <- relationship
        the.relationship.now[which.relationship] <- "Box-Cox"
        index.box.cox <- the.relationship.now == "Box-Cox"
        index.changing.power <- (1:length(relationship[index.box.cox]))[relationship[index.box.cox] !=
            "Box-Cox"]
        the.power.now <- rep(NA, length(relationship[index.box.cox]))
        the.power.now[-index.changing.power] <- groupm.out.list$groupm.out$power
  } else {
        if (replaced.relationship != "Box-Cox")
            stop(paste("Relationship", replaced.relationship,
                "cannot be replaced with a Box-Cox"))
        the.dummy.relationship <- relationship
        the.relationship.now <- relationship
        index.box.cox <- the.relationship.now == "Box-Cox"
        the.dummy.relationship[which.relationship] <- "junk"
        index.changing.power <- (1:length(relationship[index.box.cox]))[the.dummy.relationship[index.box.cox] !=
            "Box-Cox"]
        the.power.now <- groupm.out.list$groupm.out$power
    }
    index.change.xmat <- group.var[which.relationship]
    if (is.null(distribution))
        distribution <- groupm.out.list$groupm.out$distribution
    number.of.results <- length(power.vec) * length(distribution) *
        length(the.quantile.vec)
    the.results.matrix <- matrix(NA, ncol = 5, nrow = number.of.results)
    index <- 0
    result.names <- paste(rep(distribution, length(the.quantile.vec)),
        sort(rep(the.quantile.vec, length(distribution))), sep = ":")
    original.new.data <- new.data
    x.names <- colnames(xmat(data.ld))[group.var]
    if (do.scale) {
        the.replacement.xmat <- xmat(data.ld)
        the.replacement.x <- the.replacement.xmat[, index.change.xmat]
        the.max <- max(the.replacement.x)
        the.replacement.xmat[, index.change.xmat] <- the.replacement.x/the.max
        xmat(data.ld) <- the.replacement.xmat
        new.data[, which.relationship] <- new.data[, which.relationship]/the.max
        the.variable.name <- dimnames(the.replacement.xmat)[[2]][index.change.xmat]
        cat("\nFor numerical reasons, variable ", the.variable.name,
            "\nis being scaled by its maximum value ", the.max,
            ".\nThis will affect regression coefficients, \nbut not quantile/failure probability estimates.",
            sep = "")
    }
    the.log.likelihood.vec <- rep(NA, length(power.vec))
    for (i in 1:length(power.vec)) {
        the.power.now[index.changing.power] <- power.vec[i]
        for (distribution.now in distribution) {
            last.distribution <- distribution.now
            the.groupm.out <- groupm.mleprobplot(data.ld, formula = formula,
                distribution = distribution.now, relationship = the.relationship.now,
                power = the.power.now, group.var = group.var,
                plotem = plotem, compute.subsets = plotem)
            the.log.likelihood.vec[i] <- the.groupm.out$groupm.out$log.likelihood
            if (debug1) {
                cat("\n\nEstimation for lambda=", the.power.now[index.changing.power])
                print(the.groupm.out)
            }
            for (the.quantile in the.quantile.vec) {
                index <- index + 1
                names(new.data) <- x.names
                the.results.matrix[index, ] <- quantiles.groupm.out(the.groupm.out,
                  new.data = new.data, printem = F, prob.vec = the.quantile,
                  conf.level = conf.level)
            }
        }
    }
    if (do.sensitivity.plot) {
        if (!add) {
            xrna <- is.na(xlim)
            if (any(xrna))
                xlim[xrna] <- range(power.vec)[xrna]
            yrna <- is.na(ylim)
            if (any(yrna)) {
                if (length(result.names) == 1)
                  y.results.range <- the.results.matrix[, c(2,
                    4, 5)]
                else y.results.range <- the.results.matrix[,
                  c(2)]
                ylim[yrna] <- range(strip.na(y.results.range))[yrna]
            }
            if (is.null(y.axis)) {
                if (all(ylim > 0) && abs(ylim[2])/abs(ylim[1]) >
                  15)
                  y.axis <- "log"
                else y.axis <- "linear"
            }
            if (is.null(my.title)) {
                if (length(distribution) == 1)
                  title.distribution <- distribution
                else title.distribution <- ""
                my.title <- paste(get.data.title(data.ld), "\n",
                  "with", title.distribution, paste(get.x.columns(data.ld)[group.var],
                    name.relationship(relationship), sep = ":",
                    collapse = ", "), paste("at", frame.line.to.string(original.new.data)),
                  "\nPower Transformation Sensitivity Analysis on",
                  get.x.columns(data.ld)[group.var[which.relationship]])
            }
            plot.paper(xlim, ylim, x.axis = "linear", y.axis = y.axis,
                xlab = xlab, ylab = ylab, cex = cex, my.title = "",
                grids = grids, title.option = title.option, mar = c(5.1,
                  9.5, 6.5, 2.1), yaxis.line = 7)
            if (title.option != "blank")
                mtext(text = my.title, side = 3, line = 3)
        }
        index <- 0
        for (distribution.now in distribution) {
            for (the.quantile in the.quantile.vec) {
                index <- index + 1
                the.ones <- result.names == paste(distribution.now,
                  the.quantile, sep = ":")
                lines(power.vec, f.relationship(the.results.matrix[the.ones,
                  2], y.axis), lty = index)
            }
        }
        if (length(result.names) == 1) {
            lines(power.vec, f.relationship(the.results.matrix[the.ones,
                4], y.axis), lty = 3)
            lines(power.vec, f.relationship(the.results.matrix[the.ones,
                5], y.axis), lty = 3)
            cat("\n\nThe confidence interval endpoints may be crude approximations if the\ngrid of powers for evaluation is too coarse. The interval endpoints\ncould be especially misleading if the maximum of the profile\nlikelihood is not in the range of evaluation\n\n")
            if (do.legend == "On plot")
                legend(x.loc(0.003), y.loc(0.99), c(paste("ML estimate of the ",
                  the.quantile.vec, "quantile"), paste("Approximate ",
                  percent.conf.level(conf.level), "Pointwise confidence intervals")),
                  cex = 1.1, bty = "n", lty = c(1, 3),y.intersp = 0.675)
            if (do.legend == "New page") {
                plot(c(0, 0), c(1, 1), xlab = "", ylab = "",
                  type = "n", xaxt = "n", yaxt = "n")
                legend(x.loc(0.003), y.loc(0.992), c(paste("ML estimate of the ",
                  the.quantile.vec, "quantile"), paste("Approximate ",
                  percent.conf.level(conf.level), "Pointwise confidence intervals")),
                  cex = 1.1, bty = "n", lty = c(1, 3),y.intersp = 0.675)
            }
        }
        else {
            if (do.legend == "On plot")
                legend(x.loc(0.003), y.loc(0.99), paste(result.names,
                  "quantile"), cex = 1.1, bty = "n", lty = 1:length(result.names))
            if (do.legend == "New page") {
                plot(c(0, 0), c(1, 1), xlab = "", ylab = "",
                  type = "n", xaxt = "n", yaxt = "n")
                legend(x.loc(0.003), y.loc(0.99), result.names,
                  cex = 1.1, bty = "n", lty = 1:length(result.names),y.intersp = 0.675)
            }
        }
    }
    the.structure <- list(y = exp(the.log.likelihood.vec - max(the.log.likelihood.vec)),
        x = power.vec, distribution = last.distribution, number.parameters = length(the.groupm.out$groupm.out.list$groupm.out$theta.hat),
        xlab = xlab, subtitle = get.data.title(data.ld))
    invisible(the.structure)
}
