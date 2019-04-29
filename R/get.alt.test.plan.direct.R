get.alt.test.plan.direct <-
function (accel.variable.levels, number.of.units, censor.times = NULL, 
    accelvar.names = "accel.variable", describe.string = "") 
{
    accel.variable.levels <- as.matrix(accel.variable.levels)
    if (length(accelvar.names) != ncol(accel.variable.levels)) {
        accelvar.names <- paste(accelvar.names, 1:ncol(accel.variable.levels), 
            sep = "")
    }
    the.dim.names <- list(rep("", nrow(accel.variable.levels)), 
        accelvar.names)
    dimnames(accel.variable.levels) <- the.dim.names
    if (is.null(censor.times)) 
        censor.times <- rep(NA, nrow(accel.variable.levels))
    rlist <- data.frame(accel.variable.levels, number.units = number.of.units, 
        censor.times = censor.times)
    if (any(number.of.units == 0)) {
        the.positives <- number.of.units > 0
        rlist <- rlist[the.positives, ]
    }
    attr(rlist, "accelvar.names") <- accelvar.names
    attr(rlist, "levels.columns") <- vector.strip.blanks(accelvar.names, 
        FillChar = ".")
    attr(rlist, "allocation.column") <- "number.units"
    attr(rlist, "describe.string") <- describe.string
    oldClass(rlist) <- c("alt.test.plan", "data.frame")
    MysetOldClass(attr(rlist, "class"))
    return(rlist)
}
