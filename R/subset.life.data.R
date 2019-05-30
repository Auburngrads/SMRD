#' @export
subset.life.data <-
function (x, markers, columns = 1:ncol(xmat(x)),...)
{
    the.attributes <- attributes(x)
    the.attributes$data.title <- paste("subset", paste(markers,
        collapse = ","), the.attributes$data.title)
    the.attributes$residual.rmd <- NULL
    the.right.stuff <- get.the.right.stuff(x, columns,
        markers)
    the.attributes$row.names <- the.attributes$row.names[the.right.stuff]
    if (is.character(x)) {
        if (exists(x))
            the.frame <- get(envir = .frame0, x)[the.right.stuff, drop = F]
        else stop("Data set", x, "does not exist")
        the.attributes$class[the.attributes$class == "character"] <- "data.frame"
}    else {
        the.frame <- x[the.right.stuff, ]
    }
    the.vec <- the.attributes[["x.columns"]]
    ones.to.delete <- NULL
    if (!is.null(the.vec)) {
        for (i in 1:length(the.vec)) {
            if (length(unique(the.frame[, the.vec[i]])) == 1) {
                ones.to.delete <- c(ones.to.delete, i)
            }
        }
        if (!is.null(ones.to.delete))
            the.vec <- the.vec[-ones.to.delete]
        if (length(the.vec) == 0)
            the.attributes$x.columns <- NULL
        else {
            the.attributes$x.columns <- the.vec
            the.attributes$xlabel <- the.vec
        }
    }
    if (!is.null(the.attributes$censor.column)) {
        if (all(is.onlist(the.frame[, the.attributes$censor.column],
            ClistToVec(GetSMRDDefault("SMRD.FailName")))))
            the.attributes$censor.column <- NULL
    }
    attributes(the.frame) <- the.attributes
    oldClass(the.frame) <- get.life.data.class(the.frame)
    MysetOldClass(attr(the.frame, "class"))
    SMRD.sanity(the.frame)
    return(the.frame)
}
