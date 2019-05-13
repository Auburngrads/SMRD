#' @export
summary.warranty.sequence <-
function (object, max.number = NULL,...)
{
    period.list <- names(object)
    if (is.null(max.number))
        max.number <- length(object)
    for (i in 1:max.number) {
        print(period.list[i])
        print(summary(object[[period.list[i]]]$data.ld))
    }
    invisible()
}
