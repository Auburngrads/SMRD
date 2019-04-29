get.discrete.dist <-
function (distribution, mu, sigma, cut.points, prob.min = 0.001,
    prob.max = 0.999, switch = F, number.cut = 100, for.plot = F)
{
    if (switch)
        mu <- -mu
    if (missing(cut.points)) {
        the.range <- mu + c(quant(prob.min, distribution), quant(prob.max,
            distribution)) * sigma
        if (is.logdist(distribution))
            the.range <- exp(the.range)
        if (for.plot) {
            cut.points <- as.numeric(linax(the.range)$ticlab)
        }
        else {
            cut.points <- logseq(the.range[1], the.range[2],
                length = number.cut)
        }
    }
    probs <- diff(c(0, wqmf.phibf((logb(cut.points) - mu)/sigma,
        distribution)))
    total <- sum(probs)
    probs <- probs/total
    rlist <- list(x = cut.points, probs = probs)
    oldClass(rlist) <- "use.rate.dist"
    return(rlist)
}
