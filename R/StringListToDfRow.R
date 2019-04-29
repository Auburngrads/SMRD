StringListToDfRow <-
function (the.string) 
{
    hold.options <- options(warn = -1)
    on.exit(options(hold.options))
    string.vector <- ClistToVec(the.string, sep = ";")
    try.numeric <- as.numeric(string.vector)
    the.return <- data.frame(matrix("a", length(try.numeric), 
        nrow = 1))
    not.numeric <- is.na(try.numeric)
    for (i in 1:length(the.return)) {
        if (not.numeric[i]) 
            the.return[[i]] <- string.vector[i]
        else the.return[[i]] <- try.numeric[i]
    }
    return(the.return)
}
