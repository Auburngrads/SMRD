strip.char <-
function (str, char) 
{
    the.chars <- cparse(str)
    pos.of.char <- the.chars == char
    if (any(pos.of.char)) 
        return(paste(the.chars[!pos.of.char], collapse = ""))
    else return(str)
}
