check.for.old.frames <-
function (w = 3)
{
    object.list <- sort(objects( ))
    the.names <- object.list
    the.type <- rep("", length(object.list))
    lda <- rep(F, length(object.list))
    for (i in 1:length(object.list)) {
        the.object <- get(envir = .frame0, object.list[i])
        the.class <- oldClass(the.object)
        lda[i] <- is.element("life.data", the.class)
        if (lda[i]) {
            the.type[i] <- data.object.type(the.object)
        }
        if (nchar(object.list[i]) > 20)
            the.names[i] <- "xxx"
    }
    data.frame(the.names[lda], lda[lda], the.type[lda])
}
