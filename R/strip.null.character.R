strip.null.character <-
function (x) 
{
    null.space <- x == ""
    x <- x[!null.space]
    x
}
