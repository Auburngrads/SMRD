truncation.status <-
function (data.d)
{
    truncation.column <- attr(data.d, "truncation.type.column")
    if (is.null(truncation.column)) {
        return(NULL)
    }
    if (is.character(data.d) && length(data.d == 1))
        data.d <- get(envir = .frame0, data.d)
    return(data.d[[truncation.column]])
}
