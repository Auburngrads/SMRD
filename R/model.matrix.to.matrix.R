model.matrix.to.matrix <-
function (object,...)
{
    the.matrix <- matrix(NA, ncol = ncol(object), nrow = nrow(object))
    for (i in 1:ncol(the.matrix)) the.matrix[, i] <- object[,
        i]
    dimnames(the.matrix) <- dimnames(object)
    return(the.matrix)
}
