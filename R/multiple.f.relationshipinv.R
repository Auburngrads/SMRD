multiple.f.relationshipinv <-
function (x, relationship) 
{
    x <- as.matrix(x)
    fx <- x
    for (i in 1:ncol(x)) {
        switch(as.character(relationship[i]), invtemp = , temp = {
            fx <- 1000/x - 273.16
        }, invtemp3 = , temp = {
            fx <- -1000/x - 273.16
        }, reciprocal = , recip = {
            fx[, i] <- 1/x[, i]
        }, Eyring = , Arrhenius = , arrhenius = , Arrhenius2 = , 
            arrhenius2 = {
                fx[, i] <- 11604.83/x[, i] - 273.16
            }, Arrhenius3 = , arrhenius3 = {
                fx[, i] <- -11604.83/x[, i] - 273.16
            }, `Inverse Power Rule` = , Log = , log = , `inverse power rule` = {
                fx[, i] <- exp(x[, i])
            }, linear = {
                fx[, i] <- x[, i]
            }, inverse = {
                fx[, i] <- 1/x[, i]
            }, humidity = {
                fx[, i] <- 100 * plogis(x[, i])
            }, logit = {
                fx[, i] <- plogis(x[, i])
            }, logit2 = {
                fx[, i] <- 0.5 * plogis(x[, i])
            }, stop(paste(relationship[i], " is an unrecognized inverse relationship")))
    }
    return(fx)
}
