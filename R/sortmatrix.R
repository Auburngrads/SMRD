sortmatrix <-
function (data.matrix) 
{
    ind1 <- seq(1, 21, by = 4)
    ind2 <- ind1 + 1
    ind <- sort(c(ind1, ind2))
    matrix1 <- data.matrix[ind, ]
    matrix2 <- data.matrix[-ind, ]
    result <- rbind(matrix1, matrix2)
    return(result)
}
