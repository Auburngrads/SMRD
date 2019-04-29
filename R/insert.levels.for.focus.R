insert.levels.for.focus <-
function (fixed.other.values, focus.variable.index, the.xmat) 
{
    the.levels <- levels(the.xmat[, focus.variable.index])
    short.vec <- ClistToVec(fixed.other.values, sep = ";")
    long.vec <- rep("", length = length(the.levels))
    for (i in 1:length(the.levels)) {
        tmp.vec <- rep(NA, length = length(short.vec) + length(focus.variable.index))
        tmp.vec[focus.variable.index] <- the.levels[i]
        tmp.vec[-focus.variable.index] <- short.vec
        long.vec[i] <- paste(tmp.vec, collapse = ";")
    }
    return(paste(long.vec, collapse = ","))
}
