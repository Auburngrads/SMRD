get.alt.test.plan.frame <-
function (frame, levels.columns, censor.column, allocation.column,
    describe.string = "")
{
    if (!is.data.frame(frame))
        frame <- get(envir = .frame0, frame)
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    if (missing(censor.column) || is.null(censor.column)) {
        censor.times <- NULL
  } else {
        censor.column <- check.column(censor.column, ncol.data.mat,
            names.the.frame)
        censor.times <- frame[[censor.column]]
    }
    allocation.column <- check.column(allocation.column, ncol.data.mat,
        names.the.frame)
    levels.columns <- check.column(levels.columns, ncol.data.mat,
        names.the.frame, number.col.allowed = -1)
    the.levels <- frame[, levels.columns, drop = F]
    return(get.alt.test.plan.direct(accel.variable.levels = the.levels,
        number.of.units = frame[[allocation.column]], censor.times = censor.times,
        accelvar.names = names(the.levels), describe.string = describe.string))
}
