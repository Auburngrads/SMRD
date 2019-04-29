get.seq.mat <-
function (range.list, size = 5) 
{
    nparm <- length(range.list)
    index.vector <- 1:(size^nparm)
    grid.matrix <- matrix(0, nrow = length(index.vector), ncol = nparm)
    theta.matrix <- matrix(0, nrow = size, ncol = nparm)
    for (i in 1:nparm) {
        theta.vec <- seq(range.list[[i]][1], range.list[[i]][2], 
            length = size)
        theta.matrix[, i] <- theta.vec
        xx <- floor((index.vector - 1)/(size^(nparm - i)) + 1.001)
        number.vector <- (xx - 1)%%size + 1
        grid.matrix[, i] <- theta.vec[number.vector]
    }
    attr(grid.matrix, "theta.matrix") <- theta.matrix
    return(grid.matrix)
}
