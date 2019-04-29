GetShortDataPrefix <-
function (the.name) 
{
    the.length <- nchar(the.name)
    period.location <- regexpr("\\.", the.name)
    if (period.location > 0) {
        return(substring(the.name, 1, period.location - 1))
    }
    else {
        return(the.name)
    }
}
