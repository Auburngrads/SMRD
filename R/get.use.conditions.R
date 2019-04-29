get.use.conditions <-
function (data.d) 
{
    use.condition <- attr(data.d, "use.condition")
    if (is.null(use.condition)) {
        return(data.d$use.condition)
    }
    else {
        return(use.condition)
    }
}
