summary.multiple.failure.mode.life.data <-
function (object, fmode = NULL,...)
{
    the.names <- names(object)
    cat(paste("Response units: ", get.time.units(object[[1]]),
        "\n \n"))
    number.modes <- length(object)
    mode <- rep(NA, number.modes)
    number.failed <- rep(NA, number.modes)
    time.min <- rep(NA, number.modes)
    time.max <- rep(NA, number.modes)
    for (i in 1:number.modes) {
        data.ld <- object[[i]]
        y <-Response(data.ld)
        the.censor.codes <- censor.codes(data.ld)
        the.case.weights <- case.weights(data.ld)
        not.dummy <- the.case.weights > 0 & the.censor.codes >
            0
        number.cases <- nrow(y)
        failed <- not.dummy & the.censor.codes != 2
        number.failed[i] <- sum(the.case.weights[failed])
        time.min[i] <- min(y[failed])
        time.max[i] <- max(y[failed])
    }
    the.frame <- data.frame(Mode = the.names, Failed = number.failed,
        Min = time.min, Max = time.max)
    order.key <- rev(order(the.frame[, 2]))
    the.frame <- the.frame[order.key, ]
    print(the.frame)
    invisible(the.frame)
}
