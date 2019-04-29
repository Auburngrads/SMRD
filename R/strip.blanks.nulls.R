strip.blanks.nulls <-
function (xstring, maxlen = nchar(xstring)) 
{
    parse.vector <- substring(substring(xstring, 1:maxlen), 1, 
        1)
    blanks <- parse.vector == " " | parse.vector == ""
    return(paste(parse.vector[!blanks], collapse = ""))
}
