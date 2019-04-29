multiple.probplot.sim <-
function (distribution = "normal", parameter = 1, sample.size.vec = c(10,
    20, 40), number.simulations = 5, my.title = NULL)
{
    par(mar = c(5.1,4.1,4.1,2.1))
    old.par <- par(mfrow = c(length(sample.size.vec), number.simulations),
        oma = c(0, 0, 0, 0), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
        par(mar = c(5.1,4.1,4.1,2.1))
    })
    mid.plot <- floor(number.simulations/2 + 1)
    for (j in 1:length(sample.size.vec)) for (i in 1:number.simulations) {
        if (i == mid.plot) {
            print.samp.size <- T
        }
        else {
            print.samp.size <- F
        }
        xlim <- c(quant(0.001, distribution), quant(0.999,
            distribution))
        dist <- probplot.sim(distribution = distribution, parameter = parameter,
            sample.size = sample.size.vec[j], print.samp.size = print.samp.size,
            xlim = xlim)
    }
    if (is.null(my.title))
        my.title <- paste("Normal Probability Plot with Simulated ",
            dist$sim, "Data")
    mtext(side = 3, line = 0, cex = 1.5, outer = TRUE, my.title)
}
