get.data.title <-
function (data.d) 
{
    data.title <- attr(data.d, "data.title")
    
    if (is.null(data.title)) data.title <- data.d$data.title
    
    if (is.null(data.title)) data.title <- data.d$title
    
    return(data.title)
    
}
