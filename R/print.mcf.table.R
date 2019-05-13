#' @export
print.mcf.table <-
function (x,...)
{
    conf.level <- attr(x, "conf.level")
    cat(paste("\n\n     MCF table for", attr(x, "title")),
        "\n")
    cat(paste("\n                    ", percent.conf.level(conf.level),
        "Confidence Interval\n"))
    attr(x, "conf.level") <- NULL
    attr(x, "title") <- NULL
    oldClass(x) <- NULL
    print.default(x)
}
