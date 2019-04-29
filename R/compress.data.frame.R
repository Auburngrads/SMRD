compress.data.frame <-
function (the.frame, group.var = 1:ncol(the.frame), add.weights = T) 
{
    markers <- apply(the.frame[, group.var, drop = F], 1, paste, 
        sep = "", collapse = ";")
    the.dups <- duplicated(markers)
    the.matches <- match(markers, markers[!the.dups])
    the.match.frequencies <- table(the.matches)
    if (!add.weights || all(the.match.frequencies == 1)) 
        new.the.frame <- the.frame
    else new.the.frame <- cbind(the.frame[!the.dups, ], Weights = the.match.frequencies)
    return(new.the.frame)
}
