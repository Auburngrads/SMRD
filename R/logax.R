logax <-
function (xmin, xmax, nint = 5, ntick = 4, which.labels = NULL,
    ...)
{
    if (missing(xmax) && length(xmin) == 2) {
        xmax <- xmin[2]
        xmin <- xmin[1]
    }
    if (is.null(xmax)) {
        warning("xmax NULL in logax; set xmax <- .1")
        xmax <- 0.1
    }
    if (log10(xmax) >= Inf) {
        warning("xmax is too big, set equal to 10^300")
        xmax <- 10^300
    }
    if (xmax <= 0)
        stop(paste("non-positive xmax=", xmax, "in logax"))
    if (xmax < xmin)
        warning(paste("log axis xmax=", xmax, "< xmin=",
            xmin, "in logax"))
    if (xmin <= 0) {
        xmin.fix <- min(xmax/100, 1e-05)
        warning(paste("negative xmin=", xmin, "with xmax=",
            xmax, "min set to", xmin.fix))
        xmin <- xmin.fix
    }
    drange <- log10(xmax) - log10(xmin)
    if (drange > 300) {
        drange <- log10(xmax) - log10(xmin)
        warning(paste("lxmax=", xmax, "xmin=", xmin, "dynamic range=",
            drange, "is too large; restricted to 300"))
        xmean <- ((log10(xmax) + log10(xmin))/2)
        xmax <- 10^(xmean + 150)
        xmin <- 10^(xmean - 150)
        cat(paste("\nSet to xmax=", xmax, "xmin=", xmin, "\n"))
    }
    drange <- xmax/xmin
    num.dec <- floor(log10(drange)) + 4
    minmult <- floor(log10(xmin)) - 1
    mult.vec <- 10^seq(minmult, minmult + num.dec - 1)
    if (is.null(which.labels))
        which.labels <- num.dec - 1
    else {
        switch(which.labels, which.labels <- 1, which.labels <- 5,
            which.labels <- 10)
    }
    switch(as.character(which.labels), `0` = , `1` = , `2` = ,
        `3` = {
            linax.out <- linax(xmax, xmin, nint = 5, nticks = ntick)
            numlab <- as.numeric(linax.out$ticlab)
            if (any(numlab > 0)) {
                linax.out <- linax(xmax, xmin, nint = 10, nticks = ntick)
                numlab <- as.numeric(linax.out$ticlab)
            }
            numloc <- as.numeric(linax.out$ticloc)
            return(list(ticlab = format(linax.out$ticlab[numlab >
                0]), ticloc = format(linax.out$ticloc[numloc >
                0])))
        }, `5` = , `4` = {
            ticlab <- rep(c(1, 2, 5), num.dec) * sort(rep(mult.vec,
                3))
            ticloc <- rep(c(1, 1.2, 1.4, 1.6, 1.8, 2, 3, 4, 5,
                6, 7, 8, 9), num.dec) * sort(rep(mult.vec, 13))
        }, {
            ticlab <- rep(c(1), num.dec) * sort(rep(mult.vec,
                1))
            ticloc <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), num.dec) *
                sort(rep(mult.vec, 9))
        })
    xminp <- max(ticlab[ticlab <= xmin])
    xmaxp <- min(ticlab[ticlab >= xmax])
    in.range.lab <- ticlab >= xminp & ticlab <= xmaxp
    ticlab <- ticlab[in.range.lab]
    in.range.loc <- ticloc >= min(ticlab) & ticloc <= max(ticlab)
    ticloc <- ticloc[in.range.loc]
    return(list(ticlab = format(ticlab), ticloc = format(ticloc)))
}
