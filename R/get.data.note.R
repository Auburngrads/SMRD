get.data.note <-
function (data.d)
{
    data.note <- attr(data.d, "data.note")
    
    `if`(is.null(data.note),
         return(data.d$note),
         return(data.note))
}
