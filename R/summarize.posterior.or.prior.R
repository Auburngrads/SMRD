summarize.posterior.or.prior <-
function (posterior.object, task, marginal.on.pos, marginal.on.sigma, 
    type.position, newdata, include.likelihood, post.or.prior, 
    xlim = c(NA, NA), ylim = c(NA, NA), xlab = parameter.contour.axes.labels.out$xlab, 
    ylab = parameter.contour.axes.labels.out$ylab, axes.range.default.post = T, 
    factor = 4.5, size = GetSMRDDefault("SMRD.DefaultGridPoints"), 
    number.points.plot = 500) 
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    func.call <- match.call()
    switch(post.or.prior, Posterior = , post = {
        post.or.prior <- "post"
    }, Prior = , prior = {
        post.or.prior <- "prior"
    }, {
        warning(paste("bad post.or.prior--setting to post", 
            post.or.prior))
        post.or.prior <- "post"
    })
    distribution <- generic.distribution(posterior.object$distribution)
    parameter.contour.axes.labels.out <- parameter.contour.axes.labels(posterior.object$specifications.for.prior, 
        type.position)
    switch(task, `Marginal only` = , `Marginals only` = {
        if (marginal.on.sigma) {
            plot.function.task.marginals(posterior.object, post.or.prior, 
                marginal.on = "Parameter", marginal.on.detail = "spread", 
                xlim = xlim, ylim = ylim)
        }
        if (marginal.on.pos) {
            plot.function.task.marginals(posterior.object, post.or.prior, 
                marginal.on = type.position, marginal.on.detail = newdata, 
                xlim = xlim, ylim = ylim)
        }
    }, `Joint only` = {
        plot.joint.prior(posterior.object, post.or.prior, type.position = type.position, 
            newdata = newdata, xlim = xlim, ylim = ylim, 
            include.likelihood = include.likelihood, axes.range.default.post = axes.range.default.post, 
            size = size, factor = factor, number.plot = number.points.plot)
    }, Both = , `Joint w/Marginal` = , `Joint and Marginal` = {
        plot.joint.prior.or.post.marginals(posterior.object, 
            post.or.prior, type.position = type.position, newdata = newdata, 
            xlim = xlim, ylim = ylim, include.likelihood = include.likelihood, 
            axes.range.default.post = axes.range.default.post, 
            number.plot = number.points.plot)
    }, {
        stop(paste("Unrecognized task", task))
    })
    invisible()
}
