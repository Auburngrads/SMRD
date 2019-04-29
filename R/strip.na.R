strip.na <-
function (x, xcheck = as.factor(x)) 
{
    if (all(is.na(xcheck))) 
        return(NULL)
    bad.stuff <- is.na(xcheck) | xcheck == -Inf | xcheck == "NaN"
    return(x[!bad.stuff])
}
