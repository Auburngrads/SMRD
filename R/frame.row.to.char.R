frame.row.to.char <-
function (frame.row) 
{
    the.string <- rep(NA, length(frame.row))
    for (i in 1:length(frame.row)) {
        the.string[i] <- as.character(as.data.frame(frame.row)[, 
            i])
    }
    return(the.string)
}
