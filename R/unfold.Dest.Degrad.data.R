#' @export
unfold.Dest.Degrad.data <-
function (x,...)
{
    if (!is.data.frame(x))
        return(x)
    the.xmat <- xmat(x)
    if (all(unlist(lapply(the.xmat, is.numeric))))
        the.xmat <- as.matrix(the.xmat)
    the.list <- list(Response = Response(x), xmat = the.xmat,
        censor.codes = censor.codes(x), times = times(x),
        .case.weights = case.weights(x), time.units = get.time.units(x),
        response.units = get.response.units(x), data.title = get.data.title(x))
    oldClass(the.list) <- "Dest.Degrad.unfolded.data"
    return(the.list)
}
