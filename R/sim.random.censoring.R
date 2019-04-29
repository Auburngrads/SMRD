sim.random.censoring <-
function (object = "normal", censor.distribution = NULL,
    data.parameters = c(0, 1), censor.parameters = c(0, 1), sample.size = 10,
    censoring.max = NULL,...)
{
    if (length(data.parameters) != 2)
        stop("Length of data parameters must be = 2")
    yvec <- data.parameters[1] + data.parameters[2] * quant(runif(sample.size),
        object)
    if (is.null(censor.distribution)) {
        the.censor.codes <- NULL
  } else {
        if (length(censor.parameters) != 2)
            stop("Length of data parameters must be = 2")
        cvec <- censor.parameters[1] + censor.parameters[2] *
            quant(runif(sample.size), censor.distribution)
        if (!is.null(censoring.max)) {
            if (is.logdist(object))
                censoring.max < log(censoring.max)
            big.value <- cvec > censoring.max
            cvec[big.value] <- censoring.max
        }
        the.censor.codes <- rep("Fail", length = sample.size)
        censored <- yvec > cvec
        the.censor.codes[censored] <- "Censored"
        yvec[censored] <- cvec[censored]
    }
    if (is.logdist(object))
        yvec <- exp(yvec)
    yvec <- ceiling(yvec * (365.25/12))
    yvec <- yvec/(365.25/12)
    return(make.frame.ld(y = yvec, the.censor.codes = the.censor.codes,
        time.units = "Time", data.title = "Simulated Data"))
}
