get.single.dist <-
function (groupm.out, new.data)
{
    if (nrow(new.data) > 1) {
        stop("Number of rows in new.data is > 1")
    }
    relationship <- SMRD:::multiple.generic.relationship.name(groupm.out$relationship)
    if (is.null(names(relationship)))
        names(relationship) <- names(new.data)
    assign(envir = .frame0,  inherits = TRUE,"relationship.vector", relationship)
    group.var <- groupm.out$group.var
    the.xmat <- xmat(groupm.out$data.ld)[, group.var, drop = F]
    if (ncol(the.xmat) != ncol(new.data))
        stop("Number of rows in new.data does not agree with number of columns in xmat")
    the.names <- names(the.xmat)
    groupm.out$stress.names <- paste(the.names, as.matrix(new.data),
        collapse = ";", sep = "")
    add.title <- paste(" at ", groupm.out$stress.names, collapse = ";")
    Terms <- groupm.out$terms
    the.formula <- attr(Terms, "formula")
    on.exit(options(old.options))
    old.options <- options(warn = -1)
    new.data <- insert.class.xlevels(new.data, the.xmat)
    new.data.x <- model.matrix(delete.response(Terms), data = new.data,
        contrasts = Terms$contrasts, xlevels = get.class.xlevels(the.xmat,
            Terms))
    nparm <- ncol(new.data.x) + 1
    frout <- cbind(c(new.data.x, 0), c(rep(0, length = nparm -
        1), 1))
    gvec <- as.vector(groupm.out$theta.hat %*% frout)
    is.eyring <- relationship == "Eyring"
    if (any(is.eyring)) {
        eyring.relationship <- subscript.relationship(relationship,
            is.eyring)
        if (length(eyring.relationship) > 1)
            stop(paste("More than one Eyring relationship:",
                paste(relationship, collapse = ", ")))
        eyring.index <- (1:length(is.eyring))[is.eyring]
        tempk <- new.data[, eyring.index] + 273.16
        eyring.power <- attr(relationship, "the.power")[is.eyring]
        correction <- log(tempk) * eyring.power
        gvec[1] <- gvec[1] - correction
    }
    lsnames <- c("Location", "Scale")
    names(gvec) <- lsnames
    if (!is.null(groupm.out$vcv)) {
        gmat <- t(frout) %*% groupm.out$vcv %*% frout
        dimnames(gmat) <- list(lsnames, lsnames)
        values <- eigen(gmat, only.values = T)$values
        if (any(values < 0))
            warning(paste("Negative eigenvalues detected in sub covariance matrix for conditions",
                paste(unlist(new.data), collapse = ", ")))
        if (map.SMRDDebugLevel() >= 6 || any(values < 0))
            cat(" In get.single.dist, eigen values of sub.data VCV=",
                paste(format(values, digits = 3), collapse = ","),
                "\n")
    }
    else gmat <- NULL
    return(list(thetavec = gvec, vcv = gmat, add.title = add.title))
}
