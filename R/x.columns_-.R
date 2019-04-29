`x.columns<-` <-
function (data.ld, value) 
{
    frame.names <- names(data.ld)
    x.col.numbers <- match(names(data.ld[, value, drop = F]), 
        frame.names)
    if (any(is.na(x.col.numbers))) 
        stop(paste("The names", paste(value[is.na(x.col.numbers)], 
            collapse = ","), "are not in the data.frame"))
    attr(data.ld, "value") <- value
    return(data.ld)
}
