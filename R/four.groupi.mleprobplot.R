#' Title
#'
#' @param data.ld 
#' @param stresses 
#' @param group.var 
#' @param xlab 
#' @param ylab 
#' @param xlim 
#' @param ylim 
#' @param time.vector 
#' @param dump 
#' @param grids 
#' @param my.title 
#' @param cex 
#' @param linear.axes 
#' @param title.option 
#' @param pch 
#' @param lty 
#' @param ci.list 
#' @param lwd 
#' @param plot.censored.ticks 
#' @param trunc.correct 
#' @param col.fhat.vec 
#' @param col.ci 
#' @param shape 
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
#' }
four.groupi.mleprobplot <-
function (data.ld, stresses = get.x.markers(data.ld, group.var = group.var),
    group.var = 1, xlab = get.time.units(data.ld), ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
    xlim = c(NA, NA), ylim = c(NA, NA), time.vector = c(NA,
        NA), dump = 1, grids = F, my.title = NULL, cex = 1.1, linear.axes = F,
    title.option = GetSMRDDefault("SMRD.TitleOption"), pch = (1:(length(stresses) + 1))[-2],
    lty = 1:length(stresses), ci.list = NULL, lwd = rep(1, length(stresses)),
    plot.censored.ticks = F, trunc.correct = T, col.fhat.vec = 1:length(stresses),
    col.ci = 6, shape = NULL, landscape = F, ...)
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
    else title.line.adj <- 3
    distribution.list <- SMRD.FourDistributionList()
    for (i in 1:length(distribution.list)) {
        
        xlab.now <- xlab
        
        groupi.mleprobplot(data.ld = data.ld, distribution.list[i],
            stresses = stresses, group.var = group.var, ylab = "",
            xlab = xlab.now, xlim = xlim, ylim = ylim,
            time.vector = time.vector, dump = dump, grids = grids,
            my.title = "", cex = cex, linear.axes = linear.axes,
            title.option = "only.dist", pch = pch, lty = lty,
            ci.list = ci.list, lwd = lwd, plot.censored.ticks = plot.censored.ticks,
            trunc.correct = trunc.correct, col.fhat.vec = col.fhat.vec,
            col.ci = col.ci, shape = shape, do.legend = "none",
            title.line.adj = title.line.adj, ...)
    }
    mtext(text = ylab, side = 2, line = 2, outer = T, cex = 1.1)
    conf.int.title <- ""
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.ld), "\nIndividual Probability Plots",
            conf.int.title)
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
}
