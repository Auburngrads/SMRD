my.hist <-
function (x, nclass, breaks, plot = TRUE, probability = FALSE, 
    ..., xlab = deparse(substitute(x)), trim = F) 
{
    if (trim) {
        trim <- trim * length(x)
        x <- x[-c(1:trim, (length(x) - trim + 1):length(x))]
    }
    x <- x[is.finite(x)]
    if (missing(breaks)) {
        if (missing(nclass)) 
            nclass <- logb(length(x), base = 2) + 1
        breaks <- wqm.pretty(x, nclass)
        if (length(breaks) == 1) {
            if (abs(breaks) < .Machine$single.xmin * 100) 
                breaks <- c(-1, -0.5, 0.5, 1)
            else if (breaks < 0) 
                breaks <- breaks * c(1.3, 1.1, 0.9, 0.7)
            else breaks <- breaks * c(0.7, 0.9, 1.1, 1.3)
        }
        if (any(x <= breaks[1])) 
            breaks <- c(breaks[1] - diff(breaks)[1], breaks)
        x[x > max(breaks)] <- max(breaks)
    }
    bin <- cut(x, breaks)
    if (any(is.na(bin))) 
        warning("breaks do not span the range of x")
    notnabin <- !is.na(bin)
    counts <- tabulate(bin[notnabin], length(levels(bin[notnabin])))
    if (probability) {
        binw <- diff(breaks)
        if (min(binw) <= 0) 
            stop("zero width or inverted breaks")
        counts <- counts/sum(counts)/binw
    }
    if (plot) {
        if (!is.R()) {
            invisible(barplot(height = counts, width = breaks, 
                histo = T, ..., xlab = xlab))
        }
        else {
            invisible(barplot(height = counts, width = breaks, 
                space = 0, ..., xlab = xlab))
        }
    }
    else list(breaks = breaks, counts = counts)
}
