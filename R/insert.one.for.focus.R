insert.one.for.focus <-
function (fixed.other.values, focus.variable.index, relationship) 
{
    short.vec <- ClistToVec(fixed.other.values, sep = ";")
    long.vec <- rep(NA, length = length(short.vec) + length(focus.variable.index))
    long.vec[focus.variable.index] <- f.relationshipinv(1, relationship)
    long.vec[-focus.variable.index] <- short.vec
    return(paste(long.vec, collapse = ";"))
}
