compare.many.exponential.profiles <-
function (theta = 5, sample.size = 10, number.simulation = 10,
    profile.title = NULL)
{
    theta.lower <- theta * exp(-3/sqrt(sample.size))
    theta.upper <- theta * exp(4/sqrt(sample.size))
    theta.vec <- logseq(theta.lower, theta.upper, length = 1000)
    if (is.null(profile.title))
        profile.title <- paste("Relative likelihoods for simulated ",
            paste("exp(", format(theta), ")", sep = ""), " samples of size n =",
            sample.size)
    for (i in 1:number.simulation) {
        theta.hat <- sum(rexp(sample.size, 1/theta))/sample.size
        profile8 <- list(x = theta.vec, y = exponential.profile(theta.vec,
            theta.hat, sample.size), xlab = "theta")
        if (i == 1)
            add <- F
        else add <- T
        profile.plot(profile8, add = add, profile.title = profile.title,
            original.par = F)
        lines(c(theta.hat, theta.hat), y.loc(c(0.98, 1)), lwd = 2)
    }
    invisible()
}
