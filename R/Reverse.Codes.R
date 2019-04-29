Reverse.Codes <-
function (data.ld) 
{
    censor.column <- attr(data.ld, "censor.column")
    if (!is.null(censor.column)) {
        the.censor.codes <- censor.codes(data.ld)
        new.censor.status <- as.character(data.ld[[censor.column]])
        new.censor.status[the.censor.codes == 2] <- "Left"
        new.censor.status[the.censor.codes == 3] <- "Right"
        return(factor(new.censor.status))
    }
    else return(NULL)
}
