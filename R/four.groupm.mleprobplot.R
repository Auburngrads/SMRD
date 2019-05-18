#' Title
#'
#' @param data.ld 
#' @param formula 
#' @param stresses 
#' @param group.var 
#' @param xlab 
#' @param ylab 
#' @param conf.level 
#' @param xlim 
#' @param ylim 
#' @param relationship 
#' @param power 
#' @param my.title 
#' @param dump 
#' @param mle.intervals 
#' @param cex 
#' @param grids 
#' @param linear.axes 
#' @param title.option 
#' @param lwd 
#' @param plot.censored.ticks 
#' @param time.range 
#' @param shape 
#' @param ci.list 
#' @param col.ci 
#' @param printem 
#' @param trunc.correct 
#' @param new.data 
#' @param plotem 
#' @param stresses.limit 
#' @param landscape 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' DeviceA.ld <- frame.to.ld(devicea, 
#'                           data.title = "Device-A ALT Results",
#'                           response.column = 1,
#'                           time.units = "Hours",
#'                           censor.column = 2,
#'                           case.weight.column = 3,
#'                           x.columns = 4, 
#'                           xlab = "Degrees C")
#' 
#' print(DeviceA.ld)
#' summary(DeviceA.ld)
#' 
#' censored.data.plot(DeviceA.ld)
#' 
#' censored.data.plot(DeviceA.ld, 
#'                    y.axis ="log", 
#'                    x.axis = "Arrhenius")
#' 
#' groupi.mleprobplot(DeviceA.ld, 
#'                    distribution = "Weibull")
#' 
#' four.groupi.mleprobplot(DeviceA.ld)
#' 
#' DeviceA.weib.groupi <- groupi.mleprobplot(DeviceA.ld,
#'                                           distribution = "Weibull")
#' 
#' print(DeviceA.weib.groupi)
#' summary(DeviceA.weib.groupi)
#' 
#' 
#' DeviceA.lognor.groupi <- groupi.mleprobplot(DeviceA.ld, 
#'                                             distribution = "Lognormal")
#' 
#' summary(DeviceA.lognor.groupi)
#' 
#' failure.probabilities(DeviceA.lognor.groupi)
#' quantiles(DeviceA.lognor.groupi)
#' 
#' four.groupm.mleprobplot(DeviceA.ld, 
#'                         relationship = "Arrhenius")
#' 
#' }
four.groupm.mleprobplot <-
function (data.ld, formula = NULL, stresses = get.x.markers(data.ld,
    group.var = group.var), group.var = 1, xlab = get.time.units(data.ld),
    ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    xlim = c(NA, NA), ylim = c(NA, NA), relationship = NULL,
    power = NULL, my.title = NULL, dump = 0, mle.intervals = F,
    cex = 1.1, grids = F, linear.axes = F, title.option = GetSMRDDefault("SMRD.TitleOption"),
    lwd = 2, plot.censored.ticks = F, time.range = c(NA, NA),
    shape = NULL, ci.list = NULL, col.ci = 6, printem = F, trunc.correct = T,
    new.data = NULL, plotem = T, stresses.limit = 12, landscape = F,
    ...)
{
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
        SMRD.NameOnPlot = "")
    on.exit(SMRDOptions(save.SMRD.options))
    if (landscape) {
        old.par <- par(mfrow = c(2, 2), oma = c(0, 0, 0, 0),
            err = -1, mar = c(4.5, 5.1, 3.25, 2.1))
}   else {
        old.par <- par(mfcol = c(2, 2), oma = c(0, 0, 0, 0),
            err = -1, mar = c(4.5, 5.1, 3.25, 2.1))
    }
    on.exit({
        par(old.par)
        par(new = F)
        par(mfrow = c(1,1))
    })
    if (is.R())
        title.line.adj = 0.1
    else title.line.adj <- 2.8
    if (is.null(relationship)) {
        group.var.numeric <- unlist(lapply(xmat(data.ld), is.numeric))[group.var]
        relationship <- rep("linear", length = length(group.var))
        relationship[!group.var.numeric] <- "class"
    }
    if (length(relationship) != length(group.var))
        stop(paste("\nLength of relationship=", length(relationship),
            " must equal length of group.var=", length(group.var)))
    stresses.plus <- c(stresses)
    if (!is.null(new.data)) {
        new.data <- as.data.frame(new.data)
        stresses.plus <- unique(c(stresses, apply(new.data, 1,
            paste, collapse = ";")))
    }
    distribution.list <- SMRD.FourDistributionList()
    for (i in 1:length(distribution.list)) {
        
        xlab.now <- xlab
        
        if (map.SMRDDebugLevel() >= 4)
            cat("Beginning", distribution.list[i], "distribution\n")
        results <- groupm.mleprobplot(data.ld = data.ld, formula = formula,
            distribution = distribution.list[i], stresses = stresses,
            group.var = group.var, xlab = xlab.now, ylab = "",
            conf.level = conf.level, xlim = xlim, ylim = ylim,
            relationship = relationship, power = power, my.title = "",
            dump = dump, mle.intervals = mle.intervals, cex = cex,
            grids = grids, linear.axes = linear.axes, title.option = "only.dist",
            plot.censored.ticks = plot.censored.ticks, time.range = time.range,
            shape = shape, ci.list = ci.list, col.ci = col.ci,
            printem = printem, trunc.correct = trunc.correct,
            new.data = new.data, plotem = plotem, do.legend = F,
            from.six.plot = T, stresses.limit = stresses.limit,
            title.line.adj = title.line.adj, ...)
        if (!attr(results, "plotem")) {
            warning("Six plot meaningless")
            return(NULL)
        }
    }
    mtext(text = ylab, side = 2, line = 2, outer = T, cex = 1.2)
    if (!is.null(ci.list)) {
        conf.int.title <- paste("and Pointwise", percent.conf.level(conf.level),
            "Confidence Intervals")
}   else {
        conf.int.title <- ""
    }
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.ld), "\n", "Probability Plots with",
            conf.int.title, paste(name.relationship(relationship),
                collapse = ","), "Model MLE")
    }
    if (title.option == "full") {
        if (!is.R()) {
            title.line <- 2
}       else {
            title.line <- 0
        }
        mtext(side = 3, line = title.line, outer = T, text = my.title,
            cex = 1)
    }
    invisible()
}
