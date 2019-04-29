regression.profile <-
function (the.eval.vec, the.par.index = 5, theta.start = rep(NA,
    12), parameter.fixed = rep(F, 12))
{
    is.max <- function (x) { x == max(x) }
    y <- rep(NA, length = length(the.eval.vec))
    parameter.fixed <- rep(F, 12)
    theta.start <- rep(NA, 12)
    parameter.fixed[the.par.index] <- T
    for (i in 1:length(the.eval.vec)) {
        print(i)
        theta.start[the.par.index] <- the.eval.vec[i]
        the.results <- mlest(ShelfUnfld.ld, explan.vars = 1:10,
            distribution = "lognormal", parameter.fixed = parameter.fixed,
            theta.start = theta.start, kprint = 0)
        y[i] <- the.results$log.likelihood
    }
    distribution <- the.results$distribution
    max.log.like <- max(y)
    max.pos <- (1:length(y))[is.max(y)]
    cat("\nThe max is ", y[max.pos], "at", the.eval.vec[max.pos],
        "\n")
    rel <- exp(y - max.log.like)
    the.return.list <- list(x = the.eval.vec, y = rel, origy = y,
        subtitle = names(the.results$theta.hat)[the.par.index],
        xlab = names(the.results$theta.hat)[the.par.index], distribution = distribution)
    profile.plot(the.return.list)
    invisible(the.return.list)
}
