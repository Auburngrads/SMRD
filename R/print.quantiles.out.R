#' @export
print.quantiles.out <-
function (x,...)
{
    dn <- dimnames(x)
    the.dim <- dim(x)
    attributes(x) <- NULL
    dim(x) <- the.dim
    dimnames(x) <- dn
    print(x)
}
