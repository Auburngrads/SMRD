data.to.string <-
function (the.frame) 
{
    if (is.character(the.frame) && length(the.frame) == 1) 
        return(the.frame)
    if (!is.data.frame(the.frame)) 
        stop("the.frame should be a data frame, if not a new.data string")
    paste(apply(the.frame, 1, paste, collapse = ";"), collapse = ",", 
        sep = "")
}
