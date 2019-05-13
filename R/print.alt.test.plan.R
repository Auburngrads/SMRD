#' @export
print.alt.test.plan <-
function (x, digits = 4, prefix = "",...)
{
    cat("\nAccelerated life test plan \n ")
    cat(attr(x, "describe.string"), "\n")
    print.data.frame(x)
    cat("Total number.units =", sum(x$number.units), "\n")
    invisible()
}
