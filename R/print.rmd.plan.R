#' @export
print.rmd.plan <-
function (x,...)
{
    time.units <- attr(x, "time.units")
    sample.sizes <- x$sample.sizes
    time.vectors <- x$time.vectors
    if (length(sample.sizes) != length(time.vectors)) {
        print.default(x)
        stop("Corrupted x object")
    }
    for (i in 1:length(sample.sizes)) {
        sub.number.units <- sample.sizes[i]
        subtime.vec <- x$time.vectors[[i]]
        sub.number.inspections <- length(subtime.vec)
        cat("Inspection scheme", i, "has", sub.number.units,
            "units inspected at", sub.number.inspections, "inspection times\n")
        cat(paste(time.vectors[[i]], sep = ","), time.units,
            "\n\n")
    }
    invisible()
}
