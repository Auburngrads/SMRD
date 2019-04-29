groupi.Dest.Degrad <-
function (data.ddd, distribution, transformation.response, transformation.time,
    ylim = c(NA, NA), xlim = c(NA, NA), my.title = NULL,
    ylab = NULL, xlab = NULL, cex = 1.2, cex.labs = 1.5, cex.points = 1,
    add = F, grids = F, title.option = GetSMRDDefault("SMRD.TitleOption"), pch.point = NULL,
    response.on.yaxis = T, subset = T, do.legend = "On plot",
    fail.level = NULL, group.var = 1:ncol(xmat(data.ddd)), lty = NULL,
    plot.lines = T, separate.plots = F)
{
    relationship.sanity(times(data.ddd), transformation.time,
        "Transformation for Time")
    relationship.sanity(Response(data.ddd), transformation.response,
        "Transformation for Response")
    if (separate.plots) {
        results <- groupi.Dest.Degrad.indivplots(data.ddd = data.ddd,
            distribution = distribution, transformation.response = transformation.response,
            transformation.time = transformation.time, ylim = ylim,
            xlim = xlim, my.title = my.title, ylab = ylab,
            xlab = xlab, cex = cex, cex.labs = cex.labs, cex.points = cex.points,
            add = add, grids = grids, title.option = title.option,
            pch.point = pch.point, response.on.yaxis = response.on.yaxis,
            subset = subset, do.legend = do.legend, fail.level = fail.level,
            group.var = group.var, plot.lines = plot.lines)
}   else {
        results <- groupi.Dest.Degrad.oneplot(data.ddd = data.ddd,
            distribution = distribution, transformation.response = transformation.response,
            transformation.time = transformation.time, ylim = ylim,
            xlim = xlim, my.title = my.title, ylab = ylab,
            xlab = xlab, cex = cex, cex.labs = cex.labs, cex.points = cex.points,
            add = add, grids = grids, title.option = title.option,
            pch.point = pch.point, response.on.yaxis = response.on.yaxis,
            subset = subset, do.legend = do.legend, fail.level = fail.level,
            group.var = group.var, lty = lty, plot.lines = plot.lines)
    }
    invisible(results)
}
