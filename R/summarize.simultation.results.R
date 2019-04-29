summarize.simultation.results <-
function (results.object, task, marginal.on.fq1 = T, marginal.on.fq2 = T, 
    focus.quantity1, focus.quantity.detail1, x.of.interest1 = NA, 
    focus.quantity2, focus.quantity.detail2, x.of.interest2 = NA, 
    plot.type = "histogram", number.points.plot = 500, xlim = c(NA, 
        NA), ylim = c(NA, NA), my.title = NULL, xlab = NULL, 
    ylab = NULL, filter.bad = TRUE, ...) 
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
    })
    if (filter.bad) {
        ierstuff <- attr(results.object, "ierstuff")
        bad.ones <- ierstuff > 0
        if (any(bad.ones)) {
            if (sum(bad.ones) == length(bad.ones)) 
                stop(paste("All simulation", length(bad.ones), 
                  "samples were bad"))
            results.object <- results.object[!bad.ones, ]
        }
    }
    if (number.points.plot <= 0) 
        number.points.plot <- 500
    switch(task, `Marginal only` = , `Marginals only` = {
        if (marginal.on.fq1) {
            plot.marginals.sim(results.object, focus.quantity = focus.quantity1, 
                focus.quantity.detail = focus.quantity.detail1, 
                x.of.interest = x.of.interest1, plot.type = plot.type, 
                xlim = xlim, xlab = xlab, my.title = my.title, 
                ...)
        }
        if (marginal.on.fq2) {
            plot.marginals.sim(results.object, focus.quantity = focus.quantity2, 
                focus.quantity.detail = focus.quantity.detail2, 
                x.of.interest = x.of.interest2, plot.type = plot.type, 
                xlim = ylim, xlab = ylab, my.title = my.title, 
                ...)
        }
    }, `Joint only` = {
        plot.joint.sim(results.object, focus.quantity1 = focus.quantity1, 
            focus.quantity.detail1 = focus.quantity.detail1, 
            x.of.interest1 = x.of.interest1, focus.quantity2 = focus.quantity2, 
            focus.quantity.detail2 = focus.quantity.detail2, 
            x.of.interest2 = x.of.interest2, number.points.plot = number.points.plot, 
            xlim = xlim, ylim = ylim, my.title = my.title, 
            xlab = xlab, ylab = ylab, ...)
    }, Both = , `Joint w/Marginal` = , `Joint and Marginal` = {
        plot.joint.and.marginals.sim(results.object, focus.quantity1 = focus.quantity1, 
            focus.quantity.detail1 = focus.quantity.detail1, 
            x.of.interest1 = x.of.interest1, focus.quantity2 = focus.quantity2, 
            focus.quantity.detail2 = focus.quantity.detail2, 
            x.of.interest2 = x.of.interest2, number.points.plot = number.points.plot, 
            xlim = xlim, ylim = ylim, my.title = my.title, 
            xlab = xlab, ylab = ylab, plot.type = plot.type, 
            ...)
    }, Neither = {
    }, {
        stop(paste("Unrecognized task", task))
    })
    invisible()
}
