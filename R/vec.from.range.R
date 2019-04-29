vec.from.range <-
function (range, distribution, number.points)
{
    if (missing(distribution) || !is.logdist(distribution)) {
        seq(range[1], range[2], length = number.points)
    }
    else {
        logseq(range[1], range[2], length = number.points)
    }
}
