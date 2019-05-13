#' @export
predict.groupm.out <-
function (object, ProbLessThan = NA, new.data,...)
{
    relationship <- multiple.generic.relationship.name(object$relationship)
    if (is.null(names(relationship)))
        names(relationship) <- names(new.data)
    assign(envir = .frame0,  inherits = TRUE,"relationship.vector", relationship)
    group.var <- object$group.var
    data.ld <- object$data.ld
    the.xmat <- xmat(data.ld)[, group.var, drop = F]
    if (ncol(the.xmat) != ncol(new.data))
        stop("Number of columns in new.data does not agree with number of columns in xmat")
    the.names <- names(the.xmat)
    object$stress.names <- paste(the.names, as.matrix(new.data),
        collapse = ";", sep = "")
    add.title <- paste(" at ", object$stress.names, collapse = ";")
    Terms <- object$terms
    the.formula <- attr(Terms, "formula")
    on.exit(options(old.options))
    old.options <- options(warn = -1)
    new.data.lev <- insert.class.xlevels(new.data, the.xmat)
    new.data.x <- model.matrix(delete.response(Terms), data = new.data.lev,
        contrasts = Terms$contrasts, xlevels = get.class.xlevels(the.xmat,
            Terms))
    gvec <- as.vector(object$theta.hat[-length(object$theta.hat)] %*%
        t(new.data.x))
    is.eyring <- relationship == "Eyring"
    if (any(is.eyring)) {
        eyring.relationship <- subscript.relationship(relationship,
            is.eyring)
        if (length(eyring.relationship) > 1)
            stop(paste("More than one Eyring relationship:",
                paste(relationship, collapse = ", ")))
        eyring.index <- (1:length(is.eyring))[is.eyring]
        tempk <- new.data.lev[, eyring.index] + 273.16
        eyring.power <- attr(relationship, "the.power")[is.eyring]
        correction <- log(tempk) * eyring.power
        gvec <- gvec - correction
    }
    if (!is.na(ProbLessThan)) {
        response.name <- paste("Probility Less Than", ProbLessThan)
        sigma <- object$theta.hat[length(object$theta.hat)]
        if (is.logdist(object$distribution)) {
            gvec <- pnorm((log(ProbLessThan) - gvec)/sigma)
        }
        else gvec <- pnorm(ProbLessThan - gvec)/sigma
    }
    else {
        response.name <- colnames(Response(data.ld))
        if (is.logdist(object$distribution)) {
            gvec <- exp(gvec)
        }
    }
    attr(gvec, "response.name") <- response.name
    return(gvec)
}
