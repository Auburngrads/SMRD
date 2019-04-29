MapArgToNA <-
function (arg, LookFor, ReturnValue) 
{
    if (arg == LookFor || length(arg) <= 0) 
        return(ReturnValue)
    else return(arg)
}
