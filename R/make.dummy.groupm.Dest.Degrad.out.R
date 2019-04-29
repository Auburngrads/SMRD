make.dummy.groupm.Dest.Degrad.out <-
function (groupm.Dest.Degrad.out, focus.variable = 1, fixed.other.values) 
{
    data.ld <- groupm.Dest.Degrad.out$data.ld
    group.var <- groupm.Dest.Degrad.out$group.var
    model <- groupm.Dest.Degrad.out$model
    transformation.x <- fix.inverse.relationship(model$transformation.x)
    if (is.character(focus.variable)) 
        focus.variable <- match(focus.variable, names(model$xbar))
    if (is.null(fixed.other.values)) {
        tran.fixed.other.values <- matrix(model$xbar[-focus.variable], 
            ncol = length(transformation.x) - 1)
        fixed.other.values <- tran.fixed.other.values
        for (i in 1:(length(transformation.x) - 1)) {
            fixed.other.values[, i] <- f.relationshipinv(tran.fixed.other.values[, 
                i], subscript.relationship(subscript.relationship(transformation.x, 
                -focus.variable), i))
        }
    }
    else {
        fixed.other.values <- matrix(fixed.other.values, ncol = length(transformation.x) - 
            1)
    }
    x.names <- ((colnames(xmat(data.ld)))[group.var])
    theta.hat <- groupm.Dest.Degrad.out$origparam
    string.fixed.values <- paste(format(fixed.other.values), 
        " ", x.names[-focus.variable], collapse = ",")
    new.title <- paste(get.data.title(data.ld), "\nConditional on", 
        string.fixed.values)
    tran.fixed.other.values <- fixed.other.values
    for (i in 1:(length(transformation.x) - 1)) {
        tran.fixed.other.values[, i] <- f.relationship(fixed.other.values[, 
            i], subscript.relationship(subscript.relationship(transformation.x, 
            -focus.variable), i))
    }
    beta2.names <- paste("beta", seq(2, length(transformation.x) + 
        1), sep = "")
    beta2.vec <- theta.hat[beta2.names]
    beta.x <- as.matrix(tran.fixed.other.values) %*% as.matrix(beta2.vec[-focus.variable], 
        ncol = 1)
    new.theta.hat <- theta.hat[c(1, 2, focus.variable + 2, length(theta.hat))]
    new.theta.hat[2] <- new.theta.hat[2] * exp(beta.x)
    names(new.theta.hat) <- c(paste("beta", seq(0, 2), sep = ""), 
        "sigma")
    groupm.Dest.Degrad.out$origparam <- new.theta.hat
    groupm.Dest.Degrad.out$group.var <- 1
    groupm.Dest.Degrad.out$x.names <- x.names[focus.variable]
    groupm.Dest.Degrad.out$kodet <- groupm.Dest.Degrad.out$kodet[-focus.variable]
    groupm.Dest.Degrad.out$origparamvcv <- NULL
    attr(data.ld, "data.title") <- new.title
    model$transformation.x <- subscript.relationship(model$transformation.x, 
        focus.variable)
    groupm.Dest.Degrad.out$model <- model
    attr(data.ld, "x.columns") <- groupm.Dest.Degrad.out$x.names
    groupm.Dest.Degrad.out$data.ld <- data.ld
    sub.model <- attr(groupm.Dest.Degrad.out, "sub.model")
    sub.model$x.value <- sub.model$x.value[, focus.variable, 
        drop = F]
    attr(groupm.Dest.Degrad.out, "sub.model") <- sub.model
    return(groupm.Dest.Degrad.out)
}
