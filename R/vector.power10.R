vector.power10 <-
function (str.vec)
{
    retvec <- rep(NA, length(str.vec))
    for (i in seq(along = str.vec)) {
        retvec[i] <- power10(str.vec[i])
    }
    return(retvec)
}
