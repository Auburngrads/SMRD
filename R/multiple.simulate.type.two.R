multiple.simulate.type.two <-
function (n, r, distribution, number.sim = 1000)
{
    if (length(n) != length(r))
        stop(paste("Length of n=", length(n), "and r=", length(r),
            "need to be the same"))
    n.lt.2 <- n < 2
    if (any(n.lt.2))
        stop(paste("Values of n must be greater than 1",
            n[n.lt.2]))
    r.lt.2 <- r < 2
    if (any(r.lt.2))
        stop(paste("Values of r must be greater than 1",
            r[r.lt.2]))
    r.gt.n <- r > n
    if (any(r.gt.n))
        stop(paste("Values of r", r[r.gt.n], "greater than n",
            n[r.gt.n]))
    results <- list()
    for (i in 1:length(n)) {
        result.name <- paste("n=", n[i], "r=", r[i], sep = "")
        results[[result.name]] <- sim.type.two(n = n[i],
            r = r[i], distribution = distribution, number.sim = number.sim)
    }
    attr(results, "date") <- date()
    oldClass(results) <- "prob.succ.demo.type2"
    return(results)
}
