get.response.units <-
function (data.d, warn = F)
{
    response.units <- attr(data.d, "response.units")
    if (is.null(response.units))
        response.units <- attr(data.d, "time.units")
    if (is.null(response.units))
        response.units <- attr(data.d, "response.column")
    if (is.null(response.units))
        response.units <- data.d$response.units
    if (is.null(response.units)) {
        if (warn)
            warning("no response units")
        response.units <- ""
    }
    return(response.units)
}
