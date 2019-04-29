wqm.hist <-
function (x, breaks = "Sturges", freq = NULL, include.lowest = TRUE, 
    right = TRUE, density = NULL, angle = 45, col = NULL, border = NULL, 
    main = "", xlim = NULL, ylim = NULL, xlab = deparse(substitute(x)), 
    ylab = "Frequency", axes = TRUE, plot = TRUE, labels = FALSE, 
    nclass = NULL, trim = F, ...) 
{
    if (trim) {
        trim <- trim * length(x)
        x <- x[-c(1:trim, (length(x) - trim + 1):length(x))]
    }
    hist(x, breaks = breaks, freq = freq, include.lowest = include.lowest, 
            right = right, density = density, angle = angle, 
            border = border, main = main, xlab = xlab, ylab = ylab, 
            axes = axes, plot = plot, labels = labels, col = 4, 
            ...)

}
