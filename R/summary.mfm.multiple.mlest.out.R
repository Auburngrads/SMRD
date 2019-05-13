#' @export
summary.mfm.multiple.mlest.out <-
function (object,...)
{
    get.distribution <- function (x) { x$distribution }
    one.distribution <- length(unique(unlist(lapply(object,
        get.distribution)))) == 1
    if (one.distribution) {
        summary.multiple.mlest.out(object)
        return()
    }
    summary.multiple.mlest.out(object)
    invisible()
}
