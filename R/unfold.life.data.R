#' @export
unfold.life.data <-
function (x,...)
{
    if (!is.data.frame(x))
        return(x)
    if (!is.null(the.xmat <- xmat(x))) {
        the.xmat <- xmat(x)
        if (all(unlist(lapply(the.xmat, is.numeric))))
            the.xmat <- as.matrix(the.xmat)
  } else {
        the.xmat <- NULL
    }
    the.list <- list(Response = Response(x), xmat = the.xmat,
        censor.codes = censor.codes(x), .case.weights = case.weights(x),
        data.title = get.data.title(x), response.units = get.response.units(x),
        truncation.codes = truncation.codes(x), truncation.response = truncation.response(x))
    oldClass(the.list) <- c("life.unfolded.data")
    return(the.list)
}
