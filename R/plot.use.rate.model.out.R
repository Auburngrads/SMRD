#' @export
plot.use.rate.model.out <-
function (x, modes.to.plot, xlab = NULL, my.title = NULL,
    ...)
{
    plot.use.rate.model(use.rate.model = x$use.rate.model,
        field.data.ld = x$field.data.ld, lab.data.list = x$lab.data.list,
        modes.to.plot = modes.to.plot, xlab = xlab, my.title = my.title,
        ...)
}
