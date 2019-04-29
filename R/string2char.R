string2char <-
function (x) 
{
    xchar <- substring(x, 1:nchar(x), 1:nchar(x))
    xchar
}
