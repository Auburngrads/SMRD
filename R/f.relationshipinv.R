f.relationshipinv <-
function (x, relationship.in) 
{
    if (!is.character(relationship.in)) 
        warning(paste(relationship.in, "is a noncharacter relationship"))
    relationship <- generic.relationship.name(as.character(relationship.in))
    switch(relationship, invtemp = {
        fx <- 1000/x - 273.15
    }, invtemp3 = {
        fx <- -1000/x - 273.15
    }, Eyring = , Arrhenius = {
        the.arrhenius.units <- attr(relationship.in, "the.arrhenius.units")
        fx <- RecipBoltzmann(the.arrhenius.units)/x - 273.15
    }, Arrhenius3 = {
        the.arrhenius.units <- attr(relationship.in, "the.arrhenius.units")
        fx <- -RecipBoltzmann(the.arrhenius.units)/x - 273.15
    }, log = {
        fx <- exp(x)
    }, log10 = {
        fx <- 10^x
    }, class = , linear = {
        fx <- x
    }, squareroot = {
        fx <- (x)^2
    }, `Box-Cox` = {
        the.power <- attr(relationship.in, "the.power")
        if (the.power == 0) fx <- exp(x) else fx <- (x * the.power + 
            1)^(1/the.power)
    }, inverse = {
        fx <- 1/x
    }, inverse3 = {
        fx <- -1/x
    }, humidity = {
        fx <- 100 * plogis(x)
    }, logit = {
        fx <- plogis(x)
    }, logit2 = {
        fx <- 0.5 * plogis(x)
    }, stop(paste(relationship, " is an unrecognized inverse relationship")))
    return(fx)
}
