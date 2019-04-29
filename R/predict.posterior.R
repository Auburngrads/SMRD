predict.posterior <-
function (object, time.range = NULL, x.of.interest = NULL,
    extra.explan.vars = NULL, use.rows = 1:length(object$post[,
        1]), pred.tail = 0.001, number.time.points = 100,...)
{
    use.rows.prior <- use.rows
    if (is.null(nrow(object$prior)))
        object$prior <- object$prior$prior
    if (length(use.rows.prior) > nrow(object$prior))
        use.rows.prior <- 1:length(object$prior[,
            1])
    object$prior <- as.matrix(object$prior[use.rows.prior,
        ])
    object$post <- as.matrix(object$post[use.rows,
        ])
    object$prior[, 1] <- logb(object$prior[,
        1])
    object$post[, 1] <- logb(object$post[,
        1])
    distribution <- object$distribution
    explan.vars <- c(object$explan.vars, extra.explan.vars)
    if (!is.null(explan.vars)) {
        for (i in explan.vars) {
            object$xs[i] <- f.relationship(object$xs[i],
                object$relationships[i])
            x.of.interest[i] <- f.relationship(x.of.interest[i],
                object$relationships[i])
        }
        xs.plus <- c(object$xs[explan.vars], quant(object$p,
            distribution))
        col.use <- c((explan.vars + 1), ncol(object$prior))
    }
    else {
        col.use <- c(ncol(object$prior))
        xs.plus <- c(quant(object$p, distribution))
    }
    object$prior[, 1] <- object$prior[,
        1] - as.matrix(object$prior[, col.use]) %*%
        xs.plus
    object$post[, 1] <- object$post[,
        1] - as.matrix(object$post[, col.use]) %*%
        xs.plus
    col.use <- col.use[1:(length(col.use) - 1)]
    if (!is.null(explan.vars)) {
        mu.at.x.of.interest.prior <- object$prior[,
            1] + as.matrix(object$prior[, col.use]) %*%
            x.of.interest
        mu.at.x.of.interest.post <- object$post[,
            1] + as.matrix(object$post[, col.use]) %*%
            x.of.interest
    }
    else {
        mu.at.x.of.interest.prior <- object$prior[,
            1]
        mu.at.x.of.interest.post <- object$post[,
            1]
    }
    if (is.null(time.range)) {
        muaverage <- mean(mu.at.x.of.interest.post)
        sigmaaverage <- mean(object$post[, ncol(object$post)])
        time.range <- c(muaverage + quant(pred.tail, distribution) *
            sigmaaverage, muaverage + quant(1 - pred.tail, distribution) *
            sigmaaverage)
    }
    timevec <- seq(time.range[1], time.range[2], length = number.time.points)
    prediction.post <- prediction.average(timevec, mu.at.x.of.interest.post,
        object$post[, ncol(object$post)],
        distribution)
    return(prediction.post)
}
