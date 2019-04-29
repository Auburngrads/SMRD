plot.alt.fit <-
function (x, data.ld, ylim = c(NA, NA), xlim = c(NA,
    NA), xlab = NULL, ylab = NULL, grids = F, title.option = GetSMRDDefault("SMRD.TitleOption"),
    response.on.yaxis = T, include.data = T, density.at = "Automatic",
    censor.time = NULL, quant.lines = c(0.1, 0.5, 0.9), add = F,
    my.title = NULL, plot.quant.labels = T, range.of.focus = NULL,
    hw.xaxis = NULL, hw.yaxis = NULL, ...)
{
    func.call <- match.call()
    if (is.onlist("life.data", oldClass(x[[1]]))) {
        groupm.out <- x[[1]]
}   else {
        groupm.out <- x$groupm.out
    }
    variable.names <- as.list(names(xmat(groupm.out$data.ld)))
    test.for.single <- lapply(variable.names, regexpr, attr(groupm.out$terms,
        "term.labels"))
    test.for.single <- unlist(lapply(test.for.single, max)) >
        1
    if (length(test.for.single[test.for.single]) > 1)
        stop("For single explanatory variable only")
    if (missing(data.ld))
        data.ld <- groupm.out$data.ld
    if (is.null(groupm.out$focus.variable))
        groupm.out$focus.variable <- variable.names[[groupm.out$group.var]]
    if (generic.relationship.name(groupm.out$relationship) ==
        "class") {
        basic.plot.group(groupm.list = groupm.out, data.ld = data.ld, ylim = ylim,
            xlab = xlab, ylab = ylab, title.option = title.option,
            grids = grids, response.on.yaxis = response.on.yaxis,
            include.data = include.data, density.at = density.at,
            censor.time = censor.time, quant.lines = quant.lines,
            my.title = my.title, add = add, hw.xaxis = hw.xaxis,
            hw.yaxis = hw.yaxis, ...)
}   else {
        basic.plot.alt(groupm.out, data.ld, ylim = ylim,
            xlim = xlim, xlab = xlab, ylab = ylab, title.option = title.option,
            grids = grids, response.on.yaxis = response.on.yaxis,
            include.data = include.data, density.at = density.at,
            censor.time = censor.time, quant.lines = quant.lines,
            add = add, plot.quant.labels = plot.quant.labels,
            my.title = my.title, range.of.focus = range.of.focus,
            hw.xaxis = hw.xaxis, hw.yaxis = hw.yaxis, ...)
    }
    invisible()
}
