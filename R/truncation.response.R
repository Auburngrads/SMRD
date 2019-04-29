truncation.response <-
function (data.d, allow = T)
{
    frame.type <- data.object.type(data.d)
    switch(frame.type[1], frame.centered = {
        truncation.response.column <- attr(data.d, "truncation.response.column")
        if (is.null(truncation.response.column)) return(NULL)
        if (is.character(data.d) && length(data.d == 1)) data.d <- get(envir = .frame0, data.d)
        the.truncation.response <- as.matrix(data.d[, truncation.response.column])
        col.names <- dimnames(data.d)[[2]]
        names(col.names) <- col.names
        dimnames(the.truncation.response) <- list(NULL, col.names[truncation.response.column])
    }, list.centered = {
        truncation.response.column <- data.d$truncation.response.column
        if (is.null(truncation.response.column)) return(NULL)
        the.truncation.response <- data.d$frame[[truncation.response.column]]
    }, unfolded = {
        the.truncation.response <- data.d$ty
        if (is.null(the.truncation.response)) return(NULL)
        the.truncation.response <- as.matrix(truncation.response)
    }, {
        stop("Corrupted data frame")
    })
    if (!allow && is.null(the.truncation.response))
        stop("Null truncation.response")
    return(the.truncation.response)
}
