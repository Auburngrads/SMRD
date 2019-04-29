wqm.transform <-
function (data.matrix) 
{
    beta <- c(0.8, 1, 1.5, 3)
    newdata.matrix <- matrix(rep(0, 96), ncol = 4, nrow = 24)
    for (i in 1:24) {
        newdata.matrix[i, 1] <- exp(data.matrix[i, 1])
        newdata.matrix[i, 3] <- exp(data.matrix[i, 3])
    }
    for (j in 1:24) {
        newdata.matrix[j, 2] <- 1/(data.matrix[j, 2])
        newdata.matrix[j, 4] <- 1/(data.matrix[j, 4])
    }
    return(newdata.matrix)
}
