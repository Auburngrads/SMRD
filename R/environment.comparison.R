environment.comparison <-
function (npoints = 201, lab.stress.base.level = 0.85, lab.stress.amplitude = 0.1, 
    frac = 0.25) 
{
    par(mar = c(5.1, 8.5, 6.5, 2.1))
    plot(c(0, 1), c(0, 1), grids = F, type = "n", xaxt = "n", 
        yaxt = "n", xlab = "Time", ylab = "Stress", cex = 1.5)
    times <- (1:npoints - 0.5)/npoints
    lab.stress.level <- rep(lab.stress.base.level, length = npoints)
    lab.shocks.times <- seq(1, npoints, length = floor(frac * 
        npoints))
    lab.stress.level[lab.shocks.times] <- lab.stress.base.level + 
        lab.stress.amplitude
    lines(times, lab.stress.level)
    env1.stress.base.level <- 0.55
    env1.stress.amplitude.mu <- log(0.07)
    env1.stress.amplitude.sigma <- 0.4
    env1.stress.level <- rep(env1.stress.base.level, length = npoints)
    env1.shocks.times <- sample(1:npoints, size = floor((frac * 
        npoints)/2))
    env1.stress.level[env1.shocks.times] <- env1.stress.base.level + 
        rlnorm(length(env1.shocks.times), meanlog = env1.stress.amplitude.mu, 
            sdlog = env1.stress.amplitude.sigma)
    lines(times, env1.stress.level)
    env2.stress.base.level <- 0.05
    env2.stress.amplitude.mu <- log(0.05)
    env2.stress.amplitude.sigma <- 0.8
    env2.stress.level <- rep(env2.stress.base.level, length = npoints)
    env2.shocks.times <- sample(1:npoints, size = floor((frac * 
        npoints)/1.2))
    env2.stress.level[env2.shocks.times] <- env2.stress.base.level + 
        rlnorm(length(env2.shocks.times), meanlog = env2.stress.amplitude.mu, 
            sdlog = env2.stress.amplitude.sigma)
    lines(times, env2.stress.level)
    text(0.770497, 0.800818, "Laboratory test")
    text(0.770497, 0.483441, "Moderate environment")
    text(0.770497, 0.00277683, "Harsh environment")
}
