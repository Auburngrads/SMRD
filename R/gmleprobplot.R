#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param plot.dist 
#' @param xlab 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param conf.level 
#' @param interactive 
#' @param original.par 
#' @param mle.quantiles 
#' @param my.title 
#' @param type 
#' @param cex 
#' @param cexlab 
#' @param sub.title 
#' @param grids 
#' @param linear.axes 
#' @param slope.axis 
#' @param print.table 
#' @param title.option 
#' @param ylab 
#' @param trunc.correct 
#' @param add 
#' @param plot.censored.ticks 
#' @param pch 
#' @param lty 
#' @param lwd 
#' @param length.time.vec 
#' @param compare.dists 
#' @param col.ci 
#' @param lwd.ci 
#' @param xxx.mle.out 
#' @param ciMethod 
#' @param extrapolate.ci 
#' @param band.type 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' lzbearing.ld <- frame.to.ld(lzbearing, response.column = 1)
#' 
#' gmleprobplot(lzbearing.ld,
#'              distribution="bisa",
#'              compare=c("Lognormal"))
#' 
#' }
gmleprobplot <-
function (data.ld, distribution, plot.dist = "lognormal", xlab = get.time.units(data.ld),
    xlim = c(NA, NA), ylim = c(NA, NA), time.range = c(NA,
        NA), conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    interactive = T, original.par = F, mle.quantiles = T, my.title = NULL,
    type = "y", cex = 1, cexlab = 1, sub.title = "", grids = F,
    linear.axes = F, slope.axis = F, print.table = F, title.option = GetSMRDDefault("SMRD.TitleOption"),
    ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), trunc.correct = T,
    add = F, plot.censored.ticks = F, pch = 16, lty = c(3, 4,
        5, 6, 7), lwd = c(2, 2, 2, 2, 2), length.time.vec = 100,
    compare.dists = NULL, col.ci = 4, lwd.ci = 2, xxx.mle.out = NULL,
    ciMethod = "normal.approx", extrapolate.ci = F, band.type = "p",
    ...)
{
    distribution <- generic.distribution(distribution)
    gmle.out <- switch(distribution, logistic = , normal = ,
        sev = , loglogistic = , lognormal = , weibull = {
            ls.mle(data.ld, distribution = distribution)
        }, egeng = {
            egeng.mle(data.ld)
        }, gamma = {
            Gamma.mle(data.ld)
        }, igau = {
            igau.mle(data.ld)
        }, bisa = {
            bisa.mle(data.ld)
        }, sevgets = {
            gets.mle(small.interval(data.ld), distribution = "sev")
        }, levgets = {
            gets.mle(small.interval(data.ld), distribution = "lev")
        }, normalgets = {
            gets.mle(small.interval(data.ld), distribution = "normal")
        }, {
            stop("distribution not recognized")
        })
    new.gmle.out <- basic.gmleprobplot(data.ld, distribution = distribution,
        plot.dist = plot.dist, xlab = xlab, xlim = xlim,
        ylim = ylim, time.range = time.range, conf.level = conf.level,
        original.par = original.par, interactive = interactive,
        mle.quantiles = mle.quantiles, my.title = my.title, type = type,
        cex = cex, cexlab = cexlab, sub.title = sub.title, grids = grids,
        linear.axes = linear.axes, slope.axis = slope.axis, print.table = F,
        title.option = title.option, ylab = ylab, trunc.correct = trunc.correct,
        add = add, plot.censored.ticks = plot.censored.ticks,
        pch = pch, lty = lty, lwd = lwd, length.time.vec = length.time.vec,
        xxx.mle.out = gmle.out, compare.dists = compare.dists,
        band.type = band.type)
    return(new.gmle.out)
}
