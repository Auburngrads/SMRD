get.xlabel <-
function (data.d) 
{
    the.xlabel <- attr(data.d, "xlabel")
    if (is.null(the.xlabel)) 
        the.xlabel <- data.d$xlabel
    if (is.null(the.xlabel)) 
        the.xlabel <- get.x.columns(data.d)
    return(the.xlabel)
}
