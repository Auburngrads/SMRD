.write.matrix <-
function (my.matrix, file = "", append = F, include.names = T,
    include.row.names = F, digits = 4, sep = " ")
{
    the.names <- NULL
    if (is.matrix(my.matrix))
        the.names <- dimnames(my.matrix)[[2]]
    if (is.data.frame(my.matrix))
        the.names <- names(my.matrix)
    appendx <- append
    if (include.names && !is.null(the.names)) {
        cat(the.names, sep = sep, "\n", file = file, append = appendx)
        appendx <- T
    }
    save.digit <- options(digits = digits)
    on.exit(options(digits = save.digit$digits))
    if (include.row.names)
        my.matrix <- data.frame(Names = dimnames(my.matrix)[[1]],
            as.data.frame(my.matrix))
    my.matrix <- as.matrix(my.matrix)
    my.matrix <- matrix(format(my.matrix, digits = digits), ncol = ncol(my.matrix))
    .my.write(t(my.matrix), file = file, ncolumns = ncol(my.matrix),
        append = appendx, sep = sep)
    invisible()
}
