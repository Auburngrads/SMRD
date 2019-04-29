get.ADDT.test.plan.hframe <-
function (frame, levels.columns, time.columns, describe.string = "") 
{
    if (!is.data.frame(frame)) 
        frame <- get(envir = .frame0, frame)
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    time.columns <- check.column(time.columns, ncol.data.mat, 
        names.the.frame, number.col.allowed = -1)
    levels.columns <- check.column(levels.columns, ncol.data.mat, 
        names.the.frame, number.col.allowed = -1)
    the.times <- tparse(time.columns)
    time.units <- attr(the.times, "Time.units")
    the.selected.columns <- c(levels.columns, time.columns)
    frame <- frame[, the.selected.columns]
    attr(frame, "time.columns") <- time.columns
    attr(frame, "time.units") <- time.units
    attr(frame, "levels.columns") <- levels.columns
    attr(frame, "frame.type") <- "hframe"
    oldClass(frame) <- c("ADDT.test.plan", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
