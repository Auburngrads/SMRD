general.multiple.probplot.sim <-
function (paper.distribution = "normal", data.distribution = "normal", 
    censor.distribution = NULL, data.parameters = c(0, 1), censor.parameters = c(0, 
        1), sample.size.vec = c(10, 20, 40), number.simulations = 5, 
    my.title = NULL, one.size = F, SMRD.point.size = 20, censoring.max = NULL, 
    ml = T, ...) 
{
    if (one.size) {
        old.par <- par(mfrow = c(sqrt(number.simulations), sqrt(number.simulations)), 
            oma = c(0, 0, 4, 0), err = -1)
    }
    else {
        old.par <- par(mfrow = c(length(sample.size.vec), number.simulations), 
            oma = c(0, 0, 4, 0), err = -1)
    }
    oldOptions <- SMRDOptions(SMRD.point.size = SMRD.point.size)
    on.exit({
        par(old.par)
        par(new = F)
        SMRDOptions(oldOptions)
    })
    mid.plot <- floor(number.simulations/2 + 1)
    for (j in 1:length(sample.size.vec)) for (i in 1:number.simulations) {
        if (i == mid.plot && !one.size) {
            print.samp.size <- T
        }
        else {
            print.samp.size <- F
        }
        xlim <- data.parameters[1] + c(quant(0.001, data.distribution), 
            quant(0.999, data.distribution)) * data.parameters[2]
        if (is.logdist(data.distribution)) 
            xlim <- exp(xlim)
        dist <- general.probplot.sim(paper.distribution = paper.distribution, 
            data.distribution = data.distribution, censor.distribution = censor.distribution, 
            data.parameters = data.parameters, censor.parameters = censor.parameters, 
            sample.size = sample.size.vec[j], print.samp.size = print.samp.size, 
            cex = 0.7, xlim = xlim, ylim = c(NA, NA), 
            censoring.max = censoring.max, ml = ml, ...)
    }
    if (is.null(my.title)) 
        my.title <- paste(paper.distribution, "Probability Plots with Simulated ", 
            data.distribution, "Data")
    mtext(side = 3, line = 0, cex = 1.5, outer = TRUE, my.title)
}
