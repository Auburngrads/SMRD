frame.to.rdx <-
function (frame, time.column, ID.column, cost.count.column, time.units = names.the.frame[time.column[1]],
    unit.column = NULL, data.title = deparse(substitute(frame)),
    data.note = "", skip = 0)
{
    if (is.character(frame)) {
        frame <- read.table(frame, header = T, skip = skip)
        if (is.null(data.title))
            data.title <- file
    }
    else {
        if (!is.data.frame(frame))
            stop("Need to input either a frame or a file that can be read into a data frame")
    }
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    time.column <- check.column(time.column, ncol.data.mat, names.the.frame)
    if (missing(ID.column)) {
        stop("Must specify unit ID column")
    }
    else {
        ID.column <- check.column(ID.column, ncol.data.mat, names.the.frame)
    }
    if (missing(cost.count.column)) {
        stop("Must specify unit cost.count column")
    }
    else {
        cost.count.column <- check.column(cost.count.column,
            ncol.data.mat, names.the.frame)
    }
    attr(frame, "data.title") <- data.title
    attr(frame, "time.units") <- time.units
    attr(frame, "time.column") <- time.column
    attr(frame, "ID.column") <- ID.column
    attr(frame, "cost.count.column") <- cost.count.column
    attr(frame, "data.note") <- data.note
    attr(frame, "date.made") <- date()
    oldClass(frame) <- c("rdx", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
