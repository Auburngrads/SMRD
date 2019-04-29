ConditionalContourPlot <-
function (groupm.out, focus.variables, fixed.other.values = NULL,
    other.values.string = NULL, ProbLessThan = NA, xlim = c(NA,
        NA), ylim = c(NA, NA), number = 100, ...)
{
    new.data <- FocusVariableNewData(groupm.out, focus.variables = focus.variables,
        fixed.other.values = fixed.other.values, xlim = xlim,
        ylim = ylim, number = number)
    x.names <- names(new.data)
    rx <- attr(new.data, "rx")
    ry <- attr(new.data, "ry")
    the.predicted <- predict.groupm.out(groupm.out = groupm.out,
        new.data = new.data, ProbLessThan = ProbLessThan)
    response.name <- attr(the.predicted, "response.name")
    if (!is.null(fixed.other.values)) {
        other.values.string <- paste(paste(names(fixed.other.values),
            c(as.character(fixed.other.values[1, 1]), as.character(fixed.other.values[1,
                2]), as.character(fixed.other.values[1, 3])),
            sep = "="), collapse = ",  ")
    }
    else {
        if (is.null(other.values.string))
            stop("need to specify either fixed.other.values or other.values.string")
    }
    print(fixed.other.values)
    print(groupm.out)
    the.predicted[the.predicted > 10^9] <- 10^9
    if (any(is.infinite(the.predicted))) {
        cat("Infinite values in contour\n")
        print(groupm.out)
        return()
    }
    zmat <- matrix(the.predicted, nrow = number)
    contour(rx, ry, zmat, xlab = x.names[1], ylab = x.names[2],
        ...)
    data.ld <- groupm.out$data.ld
    data.title <- get.data.title(data.ld)
    title(paste(data.title, "Response:", response.name, "\n",
        other.values.string), cex = 1)
    return()
}
