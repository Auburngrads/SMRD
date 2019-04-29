plot.cdfbands <-
function (x,
          npbands,
          distribution = "uniform",
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          a.limit = 0.001,
          b.limit = 0.999,
          shape = NULL,...)
{
    ok <- x$sd > 0
    dist.probs <- x$prob[ok]
    at.point <- (x$p == x$q) & ok
    over.interval <- !(x$p == x$q) & ok

    timesp <- x$p[ok]
    timesq <- x$q[ok]

    nux.squared <- x$number.observations * (x$sd[ok]/dist.probs)^2
    kx <- nux.squared/(1 + nux.squared)
    zvalue <- 0

    if (npbands == "Simultaneous") {
        bands.over <- kx > a.limit & kx < b.limit
        zvalue <- evalue(a = a.limit, b = b.limit, conf.level = conf.level)
    }
    if (npbands == "Point-wise" || npbands == "pointwise") {
        bands.over <- kx > 0 & kx < 1
        zvalue <- qnorm(1 - (1 - conf.level)/2)
    }
    if (any(at.point)) {
        dist.probs <- x$prob[at.point]
        stderrq <- x$sd[at.point]/(dist.probs * (1 -
            dist.probs))
        lower <- plogis(qlogis(dist.probs) - zvalue * stderrq)
        upper <- plogis(qlogis(dist.probs) + zvalue * stderrq)

        points.default(x$p[at.point],
                       mono.lower(lower),
                       pch = 2,
                       cex = 2)
        points.default(x$p[at.point],
                       mono.upper(upper),
                       pch = 6,
                       cex = 2)
    }
    if (any(over.interval)) {
        dist.probs <- x$prob[over.interval]
        stderrq <- x$sd[over.interval]/(dist.probs * (1 - dist.probs))

        lower <- plogis(qlogis(dist.probs) - zvalue * stderrq)
        upper <- plogis(qlogis(dist.probs) + zvalue * stderrq)

        segments(x$p[over.interval],
                 mono.lower(lower),
                 x$q[over.interval],
                 mono.lower(lower),
                 lty = 3)

        segments(x$p[over.interval],
                 mono.upper(upper),
                 x$q[over.interval],
                 mono.upper(upper),
                 lty = 3)
    }
}
