get.data.subset <-
function (data.ld, the.right.stuff, markers = NULL)
{
    ncases <- length(the.right.stuff[the.right.stuff])
    if (ncases == 0) {
        warning(paste("0 matching in get.data.subset", paste(markers,
            collapse = ",")))
        if (map.SMRDDebugLevel() >= 4) {
            cat("the.right.stuff\n")
            print(the.right.stuff)
        }
        return(NULL)
    }
    data.ld <- data.ld[the.right.stuff, , drop = F]
    if (all(censor.codes(data.ld, warn.all.ones = F) == 1))
        censor.codes(data.ld) <- NULL
    data.title(data.ld) <- paste(get.data.title(data.ld), "subset",
        paste(markers, collapse = " "))
    right.stuff(data.ld) <- the.right.stuff
    oldClass(data.ld) <- get.life.data.class(data.ld)
    MysetOldClass(attr(data.ld, "class"))
    return(data.ld)
}
