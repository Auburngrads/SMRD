Fremaining <-
function (time.vec, FUNC, Yprime, parameters, ...) 
{
    parameter.attributes <- attributes(parameters)
    FYprime <- rep(0, length(Yprime))
    positiveYprime <- Yprime > 0
    if (any(positiveYprime)) {
        short.parameters <- parameters[positiveYprime]
        attributes(short.parameters) <- parameter.attributes
        FYprime[positiveYprime] <- FUNC(time.vec = Yprime[positiveYprime], 
            short.parameters, ...)
    }
    result <- rep(0, length(Yprime))
    positiveTime <- (time.vec + Yprime) > 0
    if (any(positiveTime)) {
        short.parameters <- parameters[positiveTime]
        attributes(short.parameters) <- parameter.attributes
        result[positiveTime] <- (FUNC(time.vec = time.vec + Yprime[positiveTime], 
            short.parameters, ...) - FYprime[positiveTime])/(1 - 
            FYprime[positiveTime])
    }
    return(result)
}
