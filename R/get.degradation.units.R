get.degradation.units <-
function (data.d) 
{
    time.units <- attr(data.d, "degradation.units")
    if (is.null(time.units)) {
        time.units <- ""
        warning("Null degradation units")
    }
    return(time.units)
}
