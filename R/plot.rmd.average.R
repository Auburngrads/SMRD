plot.rmd.average <-
function (x, xlim = c(NA, NA), ylim = c(NA, NA),
    ylab = attr(x, "response.units"), xlab = attr(x,
        "time.units"), group.var = 1:length(the.x.columns), stresses = get.x.markers(x,
        group.var = group.var), title.option = GetSMRDDefault("SMRD.TitleOption"), my.title = NULL,
    x.axis = "linear", y.axis = "linear", do.legend = "On plot",
    subset = T,...)
{
    response.units <- attr(x, "response.units")
    response.column <- attr(x, "response.column")
    time.column <- attr(x, "time.column")
    the.x.columns <- get.x.columns(x)
    unit.column <- attr(x, "unit.column")
    if (all(oldClass(x) != "repeated.measures.data"))
        stop(paste(deparse(substitute(x)), "Not a repeated measures data object"))
    subset <- get.subset.vector(subset, x)
    subset.name <- attr(subset, "subset.name")
    frame.rmd <- x[subset, ]
    Unit.marker <- frame.rmd[[unit.column]]
    if (!is.data.frame(x))
        stop("First argument must be a repeated.measures data structure")
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(x), subset.name,
            "Cell Averages", "\nx axis:", x.axis, "    y axis:",
            y.axis)
    }
    if (is.null(the.x.columns)) {
        frame.rmd <- data.frame(frame.rmd, dummy = rep(1, nrow(frame.rmd)))
        x.columns(x) <- "dummy"
        dummy.x <- T
}   else dummy.x <- F
    the.response <- as.matrix(frame.rmd[[response.column]])
    Time <- as.matrix(frame.rmd[[time.column]])
    dimnames(Time) <- list(as.character(1:nrow(Time)), "Time")
    dimnames(the.response) <- list(as.character(1:nrow(the.response)),
        "Response")
    relationship.sanity(Time, x.axis)
    relationship.sanity(the.response, y.axis)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(frame.rmd[[time.column]])[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(frame.rmd[[time.column]])[yrna]
    plot.paper(xlim, ylim, xlab = xlab, ylab = ylab, x.axis = x.axis,
        y.axis = y.axis, grids = F)
    if (title.option == "full")
        title(main = my.title, cex = 1)
    if (the.x.columns == "dummy")
        stop("Data set contains no explanatory variables")
    stresses <- get.x.markers(x, group.var = group.var)
    stress.names <- get.x.markers(x, group.var = group.var,
        long = T)
    for (i in 1:length(stresses)) {
        the.subset.x <- multiple.get.rmd.subset(x,
            stresses[i], columns = group.var)
        sub.frame <- the.subset.x
        TranResponse <- f.relationship(Response(the.subset.x),
            y.axis)
        the.list <- split(TranResponse, sub.frame[[time.column]])
        the.means <- sapply(the.list, mean)
        x <- f.relationship(as.numeric(names(the.list)), x.axis)
        orderx <- order(x)
        lines(x[orderx], the.means[orderx], lty = i, col = i)
        points.default(x[orderx], the.means[orderx], pch = i,
            col = i, cex = (1 * GetSMRDDefault("SMRD.point.size"))/100)
    }
    if (is.R()) {
        if (do.legend == "On plot" && !dummy.x)
            legend(x.loc(0.52), y.loc(0.97), stress.names, cex = 1.1,
                bty = "n", lty = 1, pch = 1:length(stress.names),
                col = 1:length(stress.names), y.intersp = 0.675)
        if (do.legend == "New page" && !dummy.x) {
            plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
                xaxt = "n", yaxt = "n")
            legend(x.loc(0.003), y.loc(0.997), stress.names,
                cex = 1.1, bty = "n", lty = 1, pch = 1:length(stress.names),
                col = 1:length(stress.names), y.intersp = 0.675)
        }
}   else {
        if (do.legend == "On plot" && !dummy.x)
            legend(x.loc(0.52), y.loc(0.97), stress.names, cex = 1.1,
                bty = "n", lty = 1, pch = 1:length(stress.names),
                col = 1:length(stress.names), y.intersp = 0.675)
        if (do.legend == "New page" && !dummy.x) {
            plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
                xaxt = "n", yaxt = "n")
            legend(x.loc(0.003), y.loc(0.997), stress.names,
                cex = 1.1, bty = "n", lty = 1, pch = 1:length(stress.names),
                col = 1:length(stress.names), y.intersp = 0.675)
        }
    }
    invisible()
}
