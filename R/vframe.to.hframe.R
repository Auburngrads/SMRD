vframe.to.hframe <-
function (hframe) 
{
    time.units <- get.time.units(hframe)
    if (is.null(time.units)) 
        time.units <- "Time"
    levels.columns <- attr(hframe, "levels.columns")
    xmat <- hframe[, levels.columns, drop = F]
    the.names <- colnames(xmat[, , drop = F])
    the.strings <- apply(xmat[, , drop = F], 1, paste, sep = "", 
        collapse = ";", the.names)
    the.unique.strings <- unique(the.strings)
    time.columns <- attr(hframe, "time.columns")
    the.times <- as.numeric(as.character(hframe[, time.columns]))
    allocation.column <- attr(hframe, "allocation.column")
    the.allocations <- as.numeric(as.character(hframe[, allocation.column]))
    the.unique.times <- sort(unique(the.times))
    allocation.matrix <- matrix(0, ncol = length(the.unique.times), 
        nrow = length(the.unique.strings))
    for (i in 1:length(the.strings)) {
        index <- match(the.strings[i], the.unique.strings)
        jndex <- match(the.times[i], the.unique.times)
        allocation.matrix[index, jndex] <- allocation.matrix[index, 
            jndex] + the.allocations[i]
    }
    dimnames(allocation.matrix) <- list(the.unique.strings, paste(time.units, 
        the.unique.times, sep = ""))
    allocation.matrix <- as.data.frame(allocation.matrix)
    return(allocation.matrix)
}
