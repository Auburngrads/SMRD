case.weights <-
function (data.d, fill.in = T)
{
    frame.type <- data.object.type(data.d)
    switch(frame.type[1], frame.centered = {
        the.case.weight.column <- attr(data.d, "case.weight.column")
        if (is.null(the.case.weight.column)) {
            if (fill.in) the.case.weights <- rep(1, length = nrow(Response(data.d))) else return(NULL)
        } else the.case.weights <- data.d[, the.case.weight.column]
    }, list.centered = {
        the.case.weight.column <- data.d$case.weight.column
        if (is.null(the.case.weight.column)) the.case.weights <- rep(1,
            length = nrow(Response(data.d))) else the.case.weights <- data.d$frame[,
            the.case.weight.column]
    }, unfolded = {
        if (any(names(data.d) == ".case.weights") && !is.null(data.d$.case.weights)) the.case.weights <- data.d$.case.weights else the.case.weights <- rep(1,
            length = nrow(Response(data.d)))
    }, {
        stop("Corrupted data frame")
    })
    if (is.null(the.case.weights))
        stop("Null .case.weights")
    return(the.case.weights)
}
