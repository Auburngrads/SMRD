ichar <-
function (strings) 
{
    .C("ichar", as.character(strings), length(strings), integer(sum(nchar(strings))))[[3]]
}
