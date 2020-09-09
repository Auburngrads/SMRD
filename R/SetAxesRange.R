SetAxesRange <-
function (type, x.axis, xlim, xlab, y.axis, ylim, ylab) 
{
    switch(type, probplot.setup = {
        file.name <- ".axes.probplot.setup"
    }, event.plot.setup = {
        file.name <- ".axes.event.plot.setup"
    }, plot.paper = {
        file.name <- ".axes.plot.paper"
    }, {
        stop(paste(type, "is not recognized\n"))
    })
    if (exists(file.name)) {
        the.list <- get(envir = .frame0, file.name)
    }
    else the.list <- NULL
    the.axes.name <- casefold(paste(x.axis, xlab, y.axis, ylab, 
        sep = ""))
    the.list[[the.axes.name]] <- list(xlim = xlim, x.axis = x.axis, 
        xlab = xlab, ylim = ylim, y.axis = y.axis, ylab = ylab)
    assign(envir = .frame0, inherits = !TRUE,file.name, the.list)
    invisible()
}
