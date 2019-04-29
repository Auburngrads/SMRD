strip.s <-
function (str) 
{
    the.chars <- cparse(str)
    if (the.chars[length(the.chars)] == "s") 
        return(paste(the.chars[-length(the.chars)], collapse = ""))
    else return(str)
}
