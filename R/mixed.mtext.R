mixed.mtext <-
function (side = 3, line = 0, outer. = F, at, texts, esc = "~", 
    cex = par("cex"), font = par("font"), srt = 0, slope, noprint = F, 
    fill.zero = T, adj = 0) 
{
    texts <- paste(esc, "B", adj * mixed.text(x = 1, y = 1, texts, 
        esc, cex, font, srt, noprint = T, fill.zero = fill.zero, 
        adj = 0), esc, ".", texts, sep = "")
    old.axt <- par(xaxt = "s", yaxt = "s")
    on.exit(par(old.axt))
    old.cex <- par("cex")
    xside <- side == 3 || side == 1
    if (missing(at)) {
        at <- ifelse(xside, mean(par("usr")[1:2]), mean(par("usr")[3:4]))
    }
    else {
        if ((xside && old.axt$xaxt == "l") || (!xside && old.axt$yaxt == 
            "l")) {
            at <- log10(at)
        }
    }
    if (missing(srt)) 
        srt <- ifelse(xside, 0, 90)
    x <- switch(side, at, par("usr")[1] - old.cex * par("1em")[1] * 
        (line + 1), at, par("usr")[2] + old.cex * par("1em")[1] * 
        (line + 1))
    y <- switch(side, par("usr")[3] - old.cex * par("1em")[2] * 
        (line + 1), at, par("usr")[4] + old.cex * par("1em")[2] * 
        (line + 1), at)
    nfont <- length(font)
    if (missing(slope)) 
        mixed.text(x, y, texts, esc, cex, font, srt = srt, noprint = noprint, 
            fill.zero = fill.zero)
    else mixed.text(x, y, texts, esc, cex, font, slope = slope, 
        noprint = noprint, fill.zero = fill.zero)
}
