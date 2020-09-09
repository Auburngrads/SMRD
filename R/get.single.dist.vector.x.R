get.single.dist.vector.x <-
function (groupm.out, new.data, do.vcv = T)
{
    group.var <- groupm.out$group.var
    the.xmat <- xmat(groupm.out$data.ld)[, group.var, drop = F]
    if (ncol(the.xmat) != ncol(new.data))
        stop("Length of new.data does not agree with number of columns in xmat")
    the.names <- names(the.xmat)
    groupm.out$stress.names <- unique(apply(as.matrix(new.data),
        1, paste, the.names, sep = "", collapse = ";"))
    add.title <- paste(" at ", groupm.out$stress.names)
    Terms <- groupm.out$terms
    the.formula <- attr(Terms, "formula")
    relationship <- multiple.generic.relationship.name(groupm.out$relationship)
    assign(envir = .frame0, inherits = !TRUE,"relationship.vector", relationship)
    new.data.x <- model.matrix(delete.response(Terms), data = new.data,
        contrasts = Terms$contrasts, xlevels = get.class.xlevels(the.xmat,
            Terms))
    nparm <- ncol(new.data.x) + 1
    frout <- array(c(as.vector(rbind(t(new.data.x), sigma = 0)),
        as.vector(rbind(matrix(0, nrow = nparm - 1, ncol = nrow(new.data)),
            rep(1, length = nrow(new.data))))), c(nparm, nrow(new.data),
        2))
    theta.hat <- t(apply(aperm(frout), 2, "%*%", groupm.out$theta.hat))
    dimnames(theta.hat) <- list(NULL, c("Location", "Scale"))
    if (do.vcv) {
        sigma.mat <- matrix(NA, ncol = 3, nrow = nrow(new.data))
        for (i in 1:nrow(new.data)) {
            gmat <- t(frout[, i, ]) %*% groupm.out$vcv %*% frout[,
                i, ]
            sigma.mat[i, ] <- gmat[c(1, 2, 4)]
        }
        dimnames(sigma.mat) <- list(NULL, c("Location", "LocationScale",
            "Scale"))
    }
    else sigma.mat <- NULL
    return(list(theta.hat = theta.hat, sigma.mat = sigma.mat,
        add.title = add.title))
}
