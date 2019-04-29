get.ALT.allocation.matrix <-
function (accvar.list, censor.time, number.of.units = 1) 
{
    the.frame <- expand.grid(accvar.list)
    levels.columns <- names(the.frame)
    the.allocations <- matrix(as.numeric(number.of.units), ncol = 1, 
        nrow = nrow(the.frame))
    the.frame <- data.frame(the.frame, rep(as.numeric(as.character(censor.time)), 
        nrow(the.frame)), the.allocations)
    for (i in 1:ncol(the.frame)) {
        the.frame[, i] <- as.numeric(as.character(the.frame[, 
            i]))
    }
    attr(the.frame, "levels.columns") <- levels.columns
    attr(the.frame, "frame.type") <- "hframe"
    dimnames(the.frame) <- list(seq(1:nrow(the.frame)), c(levels.columns, 
        "CensorTime", "Weights"))
    oldClass(the.frame) <- c("A.test.plan", "data.frame")
    MysetOldClass(attr(the.frame, "class"))
    return(the.frame)
}
