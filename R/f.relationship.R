f.relationship <-
function (x, relationship.in) 
{
    if (is.null(relationship.in)) stop("Null relationship*****")
    if (relationship.in == "") stop("Zero length character relationship*****")
    relationship <- generic.relationship.name(relationship.in)
    switch(as.character(relationship), invtemp = {
        fx <- 1000/(x + 273.15)
    }, invtemp3 = {
        fx <- -1000/(x + 273.15)
    }, Eyring = , Arrhenius = {
        the.arrhenius.units <- attr(relationship.in, "the.arrhenius.units")
        fx <- RecipBoltzmann(the.arrhenius.units)/(x + 273.15)
    }, Arrhenius3 = {
        the.arrhenius.units <- attr(relationship.in, "the.arrhenius.units")
        fx <- -RecipBoltzmann(the.arrhenius.units)/(x + 273.15)
    }, log = {
        if (any(x <= 0)) warning("Attempting to take the log of 0 or a negative number")
        fx <- logb(x)
    }, log10 = {
        if (any(x <= 0)) warning("Attempting to take the log of 0 or a negative number")
        fx <- log10(x)
    }, squareroot = {
        if (any(x < 0)) warning("Attempting to take the square root of a negative number")
        fx <- sqrt(x)
    }, `Box-Cox` = {
        the.power <- attr(relationship.in, "the.power")
        if (the.power == 0) {
            if (any(x <= 0)) {
                warning(paste("Attempting to take the log of 0 or a negative number", 
                  paste(x, collapse = ",")))
            }
            fx <- logb(x)
        } else {
            if (any(x < 0)) warning("Attempting to take a Box-Cox transformation of a negative number")
            fx <- (x^the.power - 1)/the.power
        }
    }, linear = {
        fx <- x
    }, reciprocal = {
        if (any(x == 0)) warning("Attempting to take the reciprocal of 0")
        fx <- 1/x
    }, reciprocal3 = {
        if (any(x == 0)) warning("Attempting to take the reciprocal of 0")
        fx <- -1/x
    }, humidity = {
        fx <- qlogis(x/100)
    }, logit = {
        fx <- qlogis(x)
    }, logit2 = {
        fx <- qlogis(2 * x)
    }, class = {
        fx <- dummy.variables(x)[, -1]
    }, zero = {
        fx <- 0
    }, {
        stop(paste(relationship.in, " is an unrecognized relationship"))
        fx <- NULL
    })
    return(fx)
}
