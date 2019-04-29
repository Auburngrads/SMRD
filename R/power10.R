power10 <-
function (xstring, maxlen = 10)
{
    parse.vector <- substring(substring(xstring, 1:maxlen), 1,
        1)
    e.pos <- seq(1, maxlen)[parse.vector == "e"]
    if (length(e.pos) == 0) {
        xstring <- strip.blanks.nulls(xstring)
        if (substring(xstring, 1, 1) == 0)
            xstring <- substring(xstring, 2)
        return(xstring)
    }
    null.pos <- min(seq(1, maxlen)[parse.vector == ""])
    mantissa <- substring(xstring, 1, e.pos - 1)
    power <- substring(xstring, e.pos + 1, null.pos - 1)
    if (substring(power, 1, 1) == "+")
        power <- substring(power, 2)
    if (substring(power, 1, 1) == "0")
        power <- substring(power, 2)
    if (substring(power, 2, 2) == "0" && substring(power, 1,
        1) == "-")
        power <- paste("-", substring(power, 3), sep = "", collapse = "")
    if (mantissa == "1") {
        mantissa <- ""
        xnumber <- paste("~.10~u.8~c.8~.", power, "~c1~.", sep = "")
    }
    else {
        xnumber <- paste("~.", mantissa, "x10~u.8~c.8~.", power,
            "~c1~.", sep = "")
    }
    return(xnumber)
}
