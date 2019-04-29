get.summary <-
function (data.matrix, beta) 
{
    tp <- matrix(rep(0, length(data.matrix) * 1.5), ncol = 6, 
        nrow = length(data.matrix)/4)
    k <- c(0, 1/beta, 0, 1/beta)
    mean.estimate <- c(0, 0, 0, 0)
    rmse.estimate <- c(0, 0, 0, 0)
    mean.tp <- c(0, 0, 0, 0, 0, 0)
    rmse.tp <- c(0, 0, 0, 0, 0, 0)
    for (i in 1:4) {
        mean.estimate[i] <- mean(data.matrix[, i])
        rmse.estimate[i] <- sqrt(((((length(data.matrix)/4) - 
            1)/length(data.matrix)/4) * var(data.matrix[, i])) + 
            (mean.estimate[i] - k[i])^2)
    }
    for (l in 1:length(data.matrix)/4) {
        tp[l, 1] <- exp(data.matrix[l, 1]) * (-logb(1 - 0.01))^(data.matrix[l, 
            2])
        tp[l, 2] <- exp(data.matrix[l, 1]) * (-logb(1 - 0.05))^(data.matrix[l, 
            2])
        tp[l, 3] <- exp(data.matrix[l, 1]) * (-logb(1 - 0.1))^(data.matrix[l, 
            2])
        tp[l, 4] <- exp(data.matrix[l, 3]) * (-logb(1 - 0.01))^(data.matrix[l, 
            4])
        tp[l, 5] <- exp(data.matrix[l, 3]) * (-logb(1 - 0.05))^(data.matrix[l, 
            4])
        tp[l, 6] <- exp(data.matrix[l, 3]) * (-logb(1 - 0.1))^(data.matrix[l, 
            4])
    }
    mean.tp[1] <- mean(tp[, 1])
    mean.tp[2] <- mean(tp[, 2])
    mean.tp[3] <- mean(tp[, 3])
    mean.tp[4] <- mean(tp[, 4])
    mean.tp[5] <- mean(tp[, 5])
    mean.tp[6] <- mean(tp[, 6])
    rmse.tp[1] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 1])) + (mean.tp[1] - (-logb(1 - 0.01))^(1/beta))^2)
    rmse.tp[2] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 2])) + (mean.tp[2] - (-logb(1 - 0.05))^(1/beta))^2)
    rmse.tp[3] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 3])) + (mean.tp[3] - (-logb(1 - 0.1))^(1/beta))^2)
    rmse.tp[4] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 4])) + (mean.tp[4] - (-logb(1 - 0.01))^(1/beta))^2)
    rmse.tp[5] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 5])) + (mean.tp[5] - (-logb(1 - 0.05))^(1/beta))^2)
    rmse.tp[6] <- sqrt(((((length(data.matrix)/4) - 1)/length(data.matrix)/4) * 
        var(tp[, 6])) + (mean.tp[6] - (-logb(1 - 0.1))^(1/beta))^2)
    return(list(mean.estimate = mean.estimate, rmse.estimate = rmse.estimate, 
        mean.tp = mean.tp, rmse.tp = rmse.tp))
}
