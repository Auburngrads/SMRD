profile.range <-
function (fitted, profile.on, profile.setup,...)
{
    if (length(profile.on) > 1)
        stop("profile.on is vector ")
    vcv <- fitted$t.vcv
    delta <- 0.2 * diag(vcv)
    theta.hat <- fitted$est.out$x
    profile.setup.out <- profile.setup(theta.hat, theta.hat,
        profile.on)
    profile.on.pos <- profile.setup.out$profile.on.pos
    h.theta.hat <- profile.setup.out$h.theta.hat[profile.on.pos]
    dhdt <- as.matrix(theta.hat)
    for (i in 1:length(theta.hat)) {
        theta.hat.minus <- theta.hat
        theta.hat.plus <- theta.hat
        theta.hat.minus[i] <- theta.hat.minus[i] - delta[i]
        theta.hat.plus[i] <- theta.hat.plus[i] + delta[i]
        profile.setup.plus <- profile.setup(theta.hat.plus, theta.hat,
            profile.on)
        profile.setup.minus <- profile.setup(theta.hat.minus,
            theta.hat, profile.on)
        dhdt[i, ] <- (profile.setup.plus$h.theta.hat[i] - profile.setup.minus$h.theta.hat[i])/(2 *
            delta[i])
    }
    standard.error <- sqrt(t(dhdt) %*% vcv %*% dhdt)
    return(c(h.theta.hat - 2 * standard.error, h.theta.hat +
        2 * standard.error))
}
