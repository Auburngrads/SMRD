get.allocation.matrix <-
function (accvar.list, time.units, times, reps = 1) 
{
    frame <- expand.grid(accvar.list)
    levels.columns <- names(frame)
    time.names <- paste(time.units, times, sep = "")
    the.allocations <- matrix(as.numeric(reps), ncol = length(time.names), 
        nrow = nrow(frame))
    dimnames(the.allocations) <- list(NULL, time.names)
    frame <- data.frame(frame, the.allocations)
    attr(frame, "time.columns") <- time.names
    attr(frame, "time.units") <- time.units
    attr(frame, "levels.columns") <- levels.columns
    attr(frame, "frame.type") <- "hframe"
    oldClass(frame) <- c("ADDT.test.plan", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
