simple.boot <-
function (character.input.object, simple.boot.action, simple.boot.do.probplot.summary,
    simple.boot.do.scatterplot.summary, simple.boot.do.dist.summary,
    ci.on, which.parameter, newdatalist, number.sim = 2000, number.lines.plot = 50,
    number.points.plot = 500)
{
    func.call <- match.call()
    if (simple.boot.do.dist.summary == "Compare")
        do.compare <- T
    else do.compare <- F
    mlest.out <- get(envir = .frame0, character.input.object)
    the.parameters <- names(mlest.out$theta.hat)
    data.ld <- mlest.out$data.ld
    the.boot.results <- attr(mlest.out, "the.boot.results")
    if (is.null(the.boot.results)) {
        the.boot.results <- boot.npar.par(data.ld, distribution = mlest.out$distribution,
            number.sim = number.sim)
        attr(mlest.out, "the.boot.results") <- the.boot.results
        assign(envir = .frame0, inherits = !TRUE,character.input.object, mlest.out)
    }
    the.boot.results <- boot.filter(the.boot.results)
    switch(simple.boot.action, `Summary graphics` = {
        if (!simple.boot.do.probplot.summary && !simple.boot.do.scatterplot.summary) warning("No plot choice made in dialog box.")
        if (simple.boot.do.probplot.summary) plot.boot.npar.par.out(the.boot.results,
            number.lines.plot = number.lines.plot)
        if (simple.boot.do.scatterplot.summary) plot(the.boot.results, simulate.parameters = TRUE,
            number.points.plot = number.points.plot)
    }, `Conf intervals` = {
        switch(ci.on, parameter = {
            summary.boot.npar.par.out(the.boot.results, inference.on = "parameter",
                which = match(which.parameter, the.parameters),
                do.compare = do.compare)
        }, quantile = {
            newdatalist <- as.numeric(newdatalist)
            summary.boot.npar.par.out(the.boot.results, inference.on = "quantile",
                which = newdatalist, do.compare = do.compare)
        }, `failure probability` = {
            newdatalist <- as.numeric(newdatalist)
            summary.boot.npar.par.out(the.boot.results, inference.on = "probability",
                which = newdatalist, do.compare = do.compare)
        })
    })
    return(NULL)
}
