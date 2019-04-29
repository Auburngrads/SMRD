tparse <-
function (time.names) 
{
    hold.options <- options(warn = -1)
    on.exit(options(hold.options))
    the.list <- lapply(as.list(time.names), cparse)
    the.name <- rep("", length = length(the.list))
    the.numbers <- rep(NA, length = length(the.list))
    for (i in 1:length(the.list)) {
        sub.list <- the.list[[i]]
        try.numeric <- as.numeric(sub.list)
        the.numeric <- !is.na(try.numeric)
        the.name[i] <- paste(sub.list[!the.numeric], collapse = "")
        the.numbers[i] <- as.numeric(paste(sub.list[the.numeric], 
            collapse = ""))
    }
    if (length(unique(the.name)) > 1) 
        stop(paste("Conflicting time units:", paste(unique(the.name), 
            collapse = ",")))
    attr(the.numbers, "Time.units") <- the.name[1]
    return(the.numbers)
}
