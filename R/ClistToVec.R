ClistToVec <-
function (stringlist, sep = ",")
{
    if (length(stringlist) > 1)
        stop(paste("Stringlist must be a single comma-separated string",
            paste(stringlist, collapse = " ")))
    if (length(stringlist) <= 0 || is.na(stringlist) || stringlist ==
        "NA")
        return(NA)
    the.vector <- unlist(wqm.unpaste(as.character(my.strip.blanks(stringlist)),
        sep = sep))
    the.vector <- the.vector[nchar(the.vector) > 0]
    return(the.vector)
}
