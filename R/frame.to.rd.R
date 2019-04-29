frame.to.rd <-
function (frame, response.column, censor.column, case.weight.column,
    failure.mode.column, failure.censor.names = GetSMRDDefault("SMRD.FailName"),
    right.censor.names = GetSMRDDefault("SMRD.RcName"), left.censor.names = GetSMRDDefault("SMRD.LcName"),
    interval.censor.names = GetSMRDDefault("SMRD.IcName"),
    sinterval.censor.names = GetSMRDDefault("SMRD.DefaultSintervalCensorNames"),
    data.title = deparse(substitute(frame)), time.units = names(frame)[response.column[1]],
    unit.column = NULL, data.note = "", skip = 0)
{
    if (is.character(frame)) {
        frame <- read.table(frame, header = T, skip = skip)
    }
    else {
        if (!is.data.frame(frame))
            stop("Need to input either a frame or a file that can be read into a data frame")
    }
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    check.column(response.column, ncol.data.mat, names.the.frame,
        number.col.allowed = c(1, 2))
    if (is.numeric(response.column)) {
        y <- as.matrix(frame[, response.column])
    }
    else {
        y <- as.matrix(frame[, response.column])
    }
    if (missing(censor.column)) {
        stop("Must specify unit status/event column")
    }
    else {
        check.column(censor.column, ncol.data.mat, names.the.frame)
        the.censor.codes <- ConvertToCensorCodes(as.factor(frame[,
            censor.column]), failure.censor.names, right.censor.names,
            left.censor.names, interval.censor.names, sinterval.censor.names)
    }
    if (!is.null(unit.column)) {
        check.column(unit.column, ncol.data.mat, names.the.frame,
            number.col.allowed = 1)
        unit <- as.character(frame[, unit.column])
    }
    else {
        unit <- NULL
    }
    if (missing(case.weight.column)) {
        the.case.weights <- NULL
    }
    else {
        check.column(case.weight.column, ncol.data.mat, names.the.frame)
        the.case.weights <- frame[, case.weight.column]
    }
    if (missing(failure.mode.column)) {
        the.failure.modes <- NULL
    }
    else {
        check.column(failure.mode.column, ncol.data.mat, names.the.frame)
        the.failure.modes <- frame[, failure.mode.column]
        the.char.failure.modes <- as.character(the.failure.modes)
        if (any(!is.na(match(the.char.failure.modes, ClistToVec(right.censor.names))))) {
            failure.censor.names <- paste(unique(the.char.failure.modes[is.na(match(the.char.failure.modes,
                ClistToVec(right.censor.names)))]), collapse = ",")
            if (is.null(the.censor.codes)) {
                the.censor.codes <- ConvertToCensorCodes(as.factor(frame[,
                  failure.mode.column]), failure.censor.names,
                  right.censor.names, left.censor.names, interval.censor.names,
                  sinterval.censor.names)
            }
        }
    }
    return(make.read.data(y = y), the.censor.codes = the.censor.codes,
        the.case.weights = the.case.weights, the.failure.modes = the.failure.modes,
        data.title = data.title, time.units = time.units, unit = unit,
        data.note = data.note, the.class = "recurrence.data")
}
