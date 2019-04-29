GetDataPrefix <-
function (the.name, suffix = ".ld$") 
{
    the.length <- nchar(the.name)
    if (regexpr(suffix, the.name) > 0) {
        return(substring(the.name, 1, the.length - nchar(suffix) + 
            1))
    }
    else {
        return(the.name)
    }
}
