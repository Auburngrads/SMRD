update.joint.dist <-
function (object, Wear.factor = 1.5, Crack.factor = 10,...)
{
    use.rate.model <- object$use.rate.model
    lab.parameters <- use.rate.model$lab.parameters
    lab.parameters["Wear.lab.mu.mu"] <- lab.parameters["Wear.lab.mu.mu"] +
        log(Wear.factor)
    lab.parameters["Crack.lab.mu.mu"] <- lab.parameters["Crack.lab.mu.mu"] +
        log(Crack.factor)
    use.rate.model$lab.parameters <- lab.parameters
    object$use.rate.model <- use.rate.model
    return(object)
}
