FocusVariableNewData <-
function (groupm.out, focus.variables, fixed.other.values, xlim = c(NA, 
    NA), ylim = c(NA, NA), number = 100) 
{
    variable.names <- colnames(xmat(groupm.out$data.ld))
    if (is.numeric(focus.variables)) {
        focus.variable.indices <- focus.variables
        focus.variables <- variable.names[focus.variable.indices]
    }
    else focus.variable.indices <- match(focus.variables, variable.names)
    focus.variable.relationships <- rep("", length(focus.variable.indices))
    for (i in 1:length(focus.variable.indices)) {
        focus.variable.relationships[i] <- subscript.relationship(groupm.out$relationship, 
            focus.variable.indices[i])
    }
    stresses <- wqm.unpaste(groupm.out$stresses, ";")
    x.vec <- as.numeric.nocheck(stresses[[focus.variable.indices[1]]])
    if (any(is.na(x.vec))) 
        cat("Variable", variable.names[focus.variable.indices[1]], 
            "is not numeric\n")
    y.vec <- as.numeric.nocheck(stresses[[focus.variable.indices[2]]])
    if (any(is.na(y.vec))) 
        cat("Variable", variable.names[focus.variable.indices[2]], 
            "is not numeric\n")
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(x.vec)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(y.vec)[yrna]
    rx <- seq(xlim[1], xlim[2], length = number)
    ry <- seq(ylim[1], ylim[2], length = number)
    new.data <- as.data.frame(expand.grid(rx, ry))
    names(new.data) <- variable.names[focus.variable.indices]
    new.data <- data.frame(new.data, fixed.other.values)
    attr(new.data, "rx") <- rx
    attr(new.data, "ry") <- ry
    return(new.data)
}
