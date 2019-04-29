multiple.f.relationship <-
function (xinput, relationship)
{
    x <- as.data.frame(xinput)
    the.x.names <- dimnames(xinput)[[2]]
    the.power <- attr(relationship, "the.power")
    the.arrhenius.units <- attr(relationship, "the.arrhenius.units")
    nfx <- 0
    for (i in 1:ncol(x)) {
        g.relationship.i <- generic.relationship.name(relationship[i])
        if (g.relationship.i != "class") {
            if (!is.numeric(x[, i]))
                stop(paste("Numeric variable needed for", relationship[i],
                  "relationship"))
            nfx <- nfx + 1
      } else {
            tmpx <- as.factor(x[, i])
            nfx <- nfx + length(levels(tmpx)) - 1
            if (g.relationship.i != "class") {
                warning(paste("Non-numeric explanatory variable",
                  the.x.names[i], "has relationship", relationship[i],
                  "\n but needs class relationship---reset"))
                relationship[i] <- "class"
            }
        }
    }
    fx <- matrix(NA, ncol = nfx, nrow = nrow(x))
    ifx <- 0
    for (i in 1:ncol(x)) {
        ifx <- ifx + 1
        relationship[i] <- generic.relationship.name(relationship[i])
        switch(as.character(relationship[i]), invtemp = {
            fx[, ifx] <- 1000/(x[, i] + 273.15)
        }, invtemp3 = {
            fx[, ifx] <- -1000/(x[, i] + 273.15)
        }, Eyring = , Arrhenius = {
            fx[, ifx] <- RecipBoltzmann(the.arrhenius.units[i])/(x[,
                i] + 273.15)
        }, Arrhenius3 = {
            fx[, i] <- -RecipBoltzmann(the.arrhenius.units[i])/(x[,
                i] + 273.15)
        }, log = {
            fx[, ifx] <- logb(x[, i])
        }, squareroot = {
            fx[, ifx] <- sqrt(x[, i])
        }, `Box-Cox` = {
            the.power.sub.i <- the.power[i]
            if (the.power.sub.i == 0) fx[, ifx] <- logb(x[, i]) else fx[,
                ifx] <- (x[, i]^the.power.sub.i - 1)/the.power.sub.i
        }, linear = {
            fx[, ifx] <- x[, i]
        }, reciprocal = {
            fx[, ifx] <- 1/x[, i]
        }, humidity = {
            fx[, ifx] <- qlogis(x[, i]/100)
        }, logit = {
            fx[, ifx] <- qlogis(x[, i])
        }, logit2 = {
            fx[, ifx] <- qlogis(2 * x[, i])
        }, class = {
            if (length(x[, i]) == 1) {
                dv <- dummy.variables(levels(x[, i]))[match(as.character(x[,
                  i]), levels(x[, i])), -1, drop = F]
            } else {
                dv <- dummy.variables(x[, i])[, -1, drop = F]
            }
            fx[, ifx:(ifx + ncol(dv) - 1)] <- dv
            ifx <- ifx + ncol(dv) - 1
        }, stop(paste(relationship[i], " is an unrecognized relationship")))
    }
    if (is.vector(fx))
        return(as.matrix(fx))
    else return(fx)
}
