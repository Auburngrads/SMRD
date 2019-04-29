hframe.to.vframe <-
function (hframe) 
{
    accel.var.columns <- attr(hframe, "levels.columns")
    time.columns <- attr(hframe, "time.columns")
    x.names <- names(hframe[, accel.var.columns, drop = F])
    the.x.combinations <- as.matrix(hframe[, accel.var.columns, 
        drop = F])
    the.allocation.matrix <- hframe[, time.columns, drop = F]
    the.time.names <- names(the.allocation.matrix)
    the.allocation.matrix <- as.matrix(the.allocation.matrix)
    the.times <- tparse(the.time.names)
    time.units <- attr(the.times, "Time.units")
    the.vframe <- matrix(NA, ncol = ncol(the.x.combinations) + 
        2, nrow = ncol(the.allocation.matrix) * nrow(the.allocation.matrix))
    krow <- 0
    alloc.col <- ncol(the.vframe)
    for (i in 1:nrow(the.x.combinations)) {
        for (j in 1:length(the.times)) {
            if (the.allocation.matrix[i, j] >= 0) {
                krow <- krow + 1
                the.vframe[krow, ncol(the.x.combinations) + 1] <- the.times[j]
                the.vframe[krow, alloc.col] <- as.numeric(as.character(the.allocation.matrix[i, 
                  j]))
                the.vframe[krow, 1:(ncol(the.x.combinations))] <- as.vector(the.x.combinations[i, 
                  ])
            }
        }
    }
    dimnames(the.vframe) <- list(NULL, c(x.names, time.units, 
        "Allocation"))
    the.vframe <- as.data.frame(the.vframe)
    for (j in 1:ncol(the.vframe)) {
        try.numeric <- as.numeric.nocheck(the.vframe[, j])
        if (!any(is.na(try.numeric))) 
            the.vframe[, j] <- try.numeric
    }
    oldClass(the.vframe) <- c("ADDT.vframe", "data.frame")
    MysetOldClass(attr(the.vframe, "class"))
    attr(the.vframe, "frame.type") <- "vframe"
    attr(the.vframe, "allocation.column") <- "Allocation"
    attr(the.vframe, "time.columns") <- time.units
    attr(the.vframe, "time.units") <- time.units
    attr(the.vframe, "levels.columns") <- dimnames(the.x.combinations)[[2]]
    return(the.vframe)
}
