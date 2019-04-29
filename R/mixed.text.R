mixed.text <-
function (x, y, texts, esc = "~", cex = par("cex"), font = par("font"),
    srt = 0, slope, noprint = F, fill.zero = T, adj = 0)
{
    if (noprint)
        text <- function(...) NULL
    if (adj)
        texts <- paste(esc, "B", adj * mixed.text(x, y, texts = texts,
            esc = esc, cex = cex, font = font, srt = srt, noprint = T,
            fill.zero = fill.zero, adj = 0), esc, ".", texts,
            sep = "")
    uin.x <- par("uin")[1]
    uin.y <- par("uin")[2]
    if (!missing(slope)) {
        if (!missing(srt))
            stop("only one of srt or slope may be given")
        else srt <- (180/pi) * atan((slope * uin.y)/uin.x)
    }
    srt.cos <- cos((srt * pi)/180)
    srt.sin <- sin((srt * pi)/180)
    em <- strwidth("m", 1)
    if (nchar(esc) != 1)
        stop("esc (escape character) must be single character")
    if (substring(texts, 1, 1) != esc)
        texts <- paste(esc, texts, sep = ".")
    along <- seq(nchar(texts))
    start <- (along)[substring(texts, along, along) == esc]
    if (length(start) > 1 & any(diff(start) < 2))
        stop("Consecutive escape characters unrecognized")
    end <- c(start[-1] - 1, nchar(texts))
    null.arg <- end < start + 2
    strings <- substring(texts, start + 2, pmax(end, start +
        2))
    what <- substring(texts, start + 1, start + 1)
    strings[null.arg] <- ifelse(what[null.arg] == ".", "", "1")
    numeric.arg <- as.numeric.nocheck(strings)
    if (any(bad.arg <- is.na(numeric.arg) & what != "."))
        stop(paste("invalid numeric argument:", paste(esc, what[bad.arg],
            strings[bad.arg], collapse = "", sep = "")))
    trans <- raise <- raise.cur <- 0
    cex.arg <- cex.cur <- cex
    font.cur <- font
    for (i in seq(along = what)) switch(what[i], . = , e = ,
        EXPR = {
            if (what[i] == "e") strings[i] <- paste(esc, strings[i],
                sep = "") else if (what[i] == "EXPR") strings[i] <- paste(strings[i],
                esc, sep = "")
            text(x + (srt.cos * trans - srt.sin * raise.cur)/uin.x,
                y + (srt.sin * trans + srt.cos * raise.cur)/uin.y,
                strings[i], font = font.cur, adj = 0, cex = cex.cur,
                srt = srt)
            trans <- trans + strwidth(strings[i], font = font.cur,
                cex = cex.cur, fill.zero = T)
            raise.cur <- raise
            font.cur <- font
            cex.cur <- cex
        }, f = font.cur <- numeric.arg[i], F = font <- font.cur <- numeric.arg[i],
        c = cex.cur <- numeric.arg[i] * cex.arg, C = cex <- cex.cur <- numeric.arg[i] *
            cex.arg, h = raise.cur <- numeric.arg[i] * em, H = raise <- raise.cur <- numeric.arg[i] *
            em, u = raise.cur <- raise.cur + numeric.arg[i] *
            em, U = raise.cur <- raise <- raise + numeric.arg[i] *
            em, d = raise.cur <- raise.cur - numeric.arg[i] *
            em, D = raise.cur <- raise <- raise - numeric.arg[i] *
            em, a = raise.cur <- raise.cur + 0.5 * numeric.arg[i] *
            em, A = raise.cur <- raise <- raise + 0.5 * numeric.arg[i] *
            em, v = raise.cur <- raise.cur - 0.5 * numeric.arg[i] *
            em, V = raise.cur <- raise <- raise - 0.5 * numeric.arg[i] *
            em, s = trans <- trans + 0.5 * numeric.arg[i] * em,
        S = trans <- trans + numeric.arg[i] * em, b = trans <- trans -
            0.5 * numeric.arg[i] * em, B = trans <- trans - numeric.arg[i] *
            em, warning(paste("unrecognized escape code ignored: ",
            esc, what[i], sep = "")))
    trans/em
}
