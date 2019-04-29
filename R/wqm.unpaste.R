wqm.unpaste <-
function (string, sep = "/")
{
    if (!is.character(string))
        stop(cat(string, "is not a character string in wqm.unpaste\n"))

        x <- strsplit(string, split = sep)
    return(x)
}
