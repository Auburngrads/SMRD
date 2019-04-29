map.colors <-
function (col.vec, bw = F) 
{
    return.vec <- col.vec
    if (bw) 
        sym.vec <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    else sym.vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    for (i in 1:length(col.vec)) {
        if (col.vec[i] > 9) 
            return.vec[i] <- i
        return.vec[i] <- sym.vec[col.vec[i]]
    }
    return(return.vec)
}
