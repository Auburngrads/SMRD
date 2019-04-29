events <-
function (data.d) 
{
    event.column <- get.event.column(data.d)
    if (is.null(event.column)) 
        stop("Internal error SMRD---cannot find events")
    the.events <- data.d[, event.column]
    return(the.events)
}
