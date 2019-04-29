predicted.field.data.pdf <-
function (field.cdf.pdf) 
{
    plot(range(field.cdf.pdf$tvec), 1.1 * range(field.cdf.pdf$pdf), 
        type = "n", yaxt = "n", ylab = "", xlab = "", cex = 1.2, 
        yaxs = "i")
    lines(field.cdf.pdf$tvec, field.cdf.pdf$pdf)
    title(xlab = "Weeks of Service", cex = 1.5)
    abline(v = 3 * 52)
    abline(h = 0)
    text(330.008, 0.000613896, "<- 3 years")
}
