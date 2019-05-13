#' @export
summary.FillRegion.out <-
function (object, digits = 4,...)
{
    print(object$jcr.size.time)
    FillRegion.max <- apply(object$conf.reg, 2, max)
    FillRegion.min <- apply(object$conf.reg, 2, min)
    FillRegion.len <- length(FillRegion.min)
    parameter.names <- object$gmle.out$model$t.param.names
    cat("\nLikelihood ratio confidence region points for", get.data.title(object$gmle.out$data.ld),
        "\n")
    cat("\nType of confidence region:", format(object$type),
        "\n")
    cat("\nConfidence level:", format(object$conf.level),
        "\n")
    cat("\nNumber of points:", format(object$jcr.size),
        "\n\n")
    cat("             ", parameter.names, "\n")
    cat("Minimum:     ", format(FillRegion.min[-FillRegion.len],
        digits = digits), "\n")
    cat("Lower bounds:", format(object$l.bounds, digits = digits),
        "\n")
    any.lower.2close <- apply(object$lower.2close, 2,
        any)
    names(any.lower.2close) <- parameter.names
    cat("Lower too close?:", any.lower.2close, "\n\n")
    cat("             ", parameter.names, "\n")
    cat("Maximum:     ", format(FillRegion.max[-FillRegion.len],
        digits = digits), "\n")
    cat("Upper bounds:", format(object$u.bounds, digits = digits),
        "\n")
    any.upper.2close <- apply(object$upper.2close, 2,
        any)
    names(any.upper.2close) <- parameter.names
    cat("Upper too close?:", any.upper.2close, "\n\n")
    invisible()
}
