frame.line.to.string <-
function (frame.line) 
{
    char.vec <- rep(NA, ncol(frame.line))
    for (i in 1:length(frame.line)) {
        the.element <- if (is.numeric(frame.line[, i])) 
            format(frame.line[, i])
        else as.character(frame.line[, i])
        char.vec[i] <- as.character(the.element)
    }
    return(paste(char.vec, collapse = ","))
}
