truncation.codes <-
function (data.d)
{
    frame.type <- data.object.type(data.d)
    switch(frame.type[1], frame.centered = {
        the.truncation.status <- truncation.status(data.d)
        if (is.null(the.truncation.status)) the.truncation.codes <- NULL else the.truncation.codes <- ConvertToCensorCodes(the.truncation.status,
            failure.censor.names = get.failure.censor.names(data.d),
            right.censor.names = get.right.censor.names(data.d),
            left.censor.names = get.left.censor.names(data.d),
            interval.censor.names = get.interval.censor.names(data.d),
            sinterval.censor.names = get.sinterval.censor.names(data.d))
    }, list.centered = {
        the.truncation.column <- data.d$truncation.column
        if (is.null(the.truncation.column)) return(NULL)
        the.truncation.codes <- data.d$frame[[the.truncation.column]]
    }, unfolded = {
        the.truncation.codes <- NULL
        if (any(names(data.d) == "tcodes") && !is.null(data.d$tcodes)) the.truncation.codes <- data.d$tcodes else {
            if (any(names(data.d) == "truncation.codes") && !is.null(data.d$truncation.codes)) the.truncation.codes <- data.d$truncation.codes
        }
    }, {
        stop("Corrupted data frame")
    })
    if (is.null(the.truncation.codes))
        return(NULL)
    return(the.truncation.codes)
}
