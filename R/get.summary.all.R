get.summary.all <-
function (data.matrix, e.of.r = c(2, 5, 10, 20, 50, 100), beta = c(0.8,
    1, 1.5, 3))
{
    estimate.mean <- matrix(rep(0, length(data.matrix) * 4),
        nrow = length(data.matrix), ncol = 4)
    estimate.rmse <- matrix(rep(0, length(data.matrix) * 4),
        nrow = length(data.matrix), ncol = 4)
    tp.mean <- matrix(rep(0, length(data.matrix) * 6), nrow = length(data.matrix),
        ncol = 6)
    tp.rmse <- matrix(rep(0, length(data.matrix) * 6), nrow = length(data.matrix),
        ncol = 6)
    for (j in 1:length(e.of.r)) {
        for (k in 1:length(beta)) {
            estimate.mean[4 * (j - 1) + k, ] <- (get.summary(data.matrix[[4 *
                (j - 1) + k]], beta = beta[k])[[1]])
            estimate.rmse[4 * (j - 1) + k, ] <- (get.summary(data.matrix[[4 *
                (j - 1) + k]], beta = beta[k])[[2]])
            tp.mean[4 * (j - 1) + k, ] <- (get.summary(data.matrix[[4 *
                (j - 1) + k]], beta = beta[k])[[1]])
            tp.rmse[4 * (j - 1) + k, ] <- (get.summary(data.matrix[[4 *
                (j - 1) + k]], beta = beta[k])[[2]])
        }
    }
    return(estimate.mean = estimate.mean, estimate.rmse = estimate.rmse,
        tp.mean = tp.mean, tp.rmse = tp.rmse)
}
