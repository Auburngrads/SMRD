matrix.filler <-
function (the.input, pos, the.filler) 
{
    if (is.null(pos)) 
        return(the.input)
    if (is.matrix(the.input)) {
        number.parameters <- ncol(the.input) + length(pos)
        mask <- (1:number.parameters)[-pos]
        hold <- matrix(the.filler, ncol = number.parameters, 
            nrow = number.parameters)
        hold[mask, mask] <- the.input
        return(hold)
    }
    else {
        if (is.vector(the.input)) {
            number.parameters <- length(the.input) + length(pos)
            mask <- (1:number.parameters)[-pos]
            if (length(the.filler) == number.parameters) {
                hold <- the.filler
            }
            else {
                hold <- rep(the.filler, number.parameters)
            }
            hold[mask] <- the.input
            return(hold)
        }
        else {
            stop("The input must be either a vector or a matrix")
        }
    }
}
