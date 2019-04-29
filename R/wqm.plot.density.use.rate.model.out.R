wqm.plot.density.use.rate.model.out <-
function (use.rate.model.out, npoints = 200, xlab = NULL, my.title = NULL,
    ...)
{
    if (is.null(my.title))
        my.title <- paste("Lab:", get.data.title(use.rate.model.out$lab.data.ld),
            "\nField:", get.data.title(use.rate.model.out$field.data.ld))
    wqm.plot.density.use.rate.model(use.rate.model.out$use.rate.model,
        npoints = npoints, xlab = xlab, my.title = my.title,
        ...)
}
