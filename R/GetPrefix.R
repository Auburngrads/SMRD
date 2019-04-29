GetPrefix <-
function (the.name) 
{
    the.length <- nchar(the.name)
    the.characters <- string2char(the.name)
    first.period <- (1:the.length)[the.characters == "."][1]
    if (!is.na(first.period)) 
        return(substring(the.name, 1, first.period - 1))
    else return(NULL)
}
