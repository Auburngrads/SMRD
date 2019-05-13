#' @export
plot.warranty.sequence <-
function (x, miles.limit = 36, time.limit = 36,
    max.number = NULL, xlim = c(2, 1000), ylim = c(2, 1000),
    six.page = T, sigma.factor = 2.5, postscript.out = T,...)
{
    if (!postscript.out) {
        if (six.page) {
            cex <- 0.6
            old.par <- par(mfrow = c(2, 3), oma = c(0, 0, 4,
                0), err = -1)
      } else {
            cex <- 1
            old.par <- par(err = -1)
        }
        on.exit({
            par(old.par)
            par(new = F)
        })
    }
    period.list <- names(x)
    if (is.null(max.number))
        max.number <- length(x)
    the.array <- array(unlist(lapply(x,
        range.bvn.gmle.out, sigma.factor = sigma.factor)), dim = c(2,
        2, 6))
    the.array.xlim <- c(apply(the.array, c(2), min)[1], apply(the.array,
        c(2), max)[1])
    the.array.ylim <- c(apply(the.array, c(2), min)[2], apply(the.array,
        c(2), max)[2])
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- the.array.xlim[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- the.array.ylim[yrna]
    for (i in 1:max.number) {
        if (postscript.out) {
            if (i%%6 == 1) {
                if (i == 1) {
                  figure.name <- paste(x[[1]]$Lcode,
                    period.list[i], "ps", sep = ".")
                  postscript(file = warranty.fig(figure.name),
                    horizontal = T)
              } else {
                  dev.off()
                  figure.name <- paste(x[[1]]$Lcode,
                    period.list[i], "ps", sep = ".")
                  postscript(file = warranty.fig(figure.name),
                    horizontal = T)
                }
                if (six.page) {
                  cex <- 0.6
                  old.par <- par(mfrow = c(2, 3), oma = c(0,
                    0, 4, 0), err = -1)
              } else {
                  cex <- 1
                  old.par <- par(err = -1)
                }
            }
        }
        print(period.list[i])
        if (!is.null(x[[period.list[i]]]$dummy) &&
            x[[period.list[i]]]$dummy ==
                T) {
            next
        }
        plot.warranty.gmle.out(x[[period.list[i]]],
            miles.limit = miles.limit, time.limit = time.limit,
            xlim = xlim, ylim = ylim, cex = cex)
        mtext(text = x[[period.list[i]]]$data.ld$title,
            side = 3, line = 2)
    }
    if (postscript.out) {
        par(old.par)
        par(new = F)
        if (names(dev.cur()) == "postscript")
            dev.off()
    }
    oldClass(x) <- "multiple.gmle.out"
    invisible(x)
}
