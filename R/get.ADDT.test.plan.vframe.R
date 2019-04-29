get.ADDT.test.plan.vframe <-
function (frame, levels.columns, times.column, allocation.column,
    describe.string = "")
{
    if (!is.data.frame(frame))
        frame <- get(envir = .frame0, frame)
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    allocation.column <- check.column(allocation.column, ncol.data.mat,
        names.the.frame)
    times.column <- check.column(times.column, ncol.data.mat,
        names.the.frame)
    levels.columns <- check.column(levels.columns, ncol.data.mat,
        names.the.frame, number.col.allowed = -1)
    attr(frame, "allocation.column") <- allocation.column
    attr(frame, "times.column") <- times.column
    attr(frame, "time.units") <- times.column
    attr(frame, "levels.columns") <- levels.columns
    attr(frame, "frame.type") <- "vframe"
    oldClass(frame) <- c("ADDT.test.plan", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
