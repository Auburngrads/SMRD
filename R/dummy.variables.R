dummy.variables <-
function (class.vec) 
{
    unique.vec <- unique(class.vec)
    vec.matrix <- matrix(0, nrow = length(class.vec), ncol = length(unique.vec))
    for (i in 1:length(unique.vec)) {
        vec.matrix[class.vec == unique.vec[i], i] <- 1
    }
    return(vec.matrix)
}
