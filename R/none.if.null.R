none.if.null <-
function (element, the.return = "None") 
{
    if (length(element) == 0 || (length(element) == 1 && element == 
        "")) 
        return(the.return)
    else return(element)
}
