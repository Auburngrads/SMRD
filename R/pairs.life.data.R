pairs.life.data <-
function (x, title.option = GetSMRDDefault("SMRD.TitleOption"), labels = dimnames(x.1)[[2]],
    minlength = c(4, 7), invert = TRUE, panel = points,...)
{
    old.par <- par()
    on.exit(par(old.par))
    the.response <- Response(x)[, 1, drop = F]
    x.1 <- data.frame(the.response, xmat(x))
    dimnames(x.1)[[2]][1] <- dimnames(the.response)[[2]][1]
    right.censored <- censor.codes(x) == 2
    n <- ncol(x.1)
    for (i in seq(n)) if (is.matrix(x.1[[i]])) {
        x.1 <- xpdmat.data.frame(x.1)
        n <- ncol(x.1)
    }
    doaxis <- function(which, axp, visible, labels, srt) {
        if (visible && !is.null(labels))
            visible <- labels
        at <- if (!is.null(labels))
            seq(axp[1] + 1, axp[2] - 1, length = length(labels))
        else seq(axp[1], axp[2], length = axp[3] + 1)
        axis(which, at = round(at, digits = 3), outer = T, line = -0.5,
            labels = visible, srt = srt)
    }
    dolabel <- if (is.vector(minlength) && length(minlength) ==
        2)
        function(x.1, minlength) abbreviate(x.1, minlength = minlength)
    else stop("bad argument for abbreviate")
    if (is.character(panel))
        panel <- get(envir = .frame0, panel, mode = "function")
    xrange <- axis.labels <- list()
    for (i in seq(n)) {
        X <- x.1[, i]
        if (is.factor(X)) {
            x.1[, i] <- I(factor(X, exclude = if (any(is.na(X)))
                NA
            else NULL))
            axis.labels[[i]] <- dolabel(levels(x.1[, i]), minlength)
            xrange[[i]] <- c(0, max(x.1[, i], na.rm = T) + 1)
        }
        else xrange[[i]] <- range(X, na.rm = T)
    }
    oldpar <- par("err", "oma", "mar", "cex", "tck", "mfg", "mgp",
        "mex", "mfrow")
    oldcex <- par("cex")
    CEX <- oldcex * max(7.7/(2 * n + 3), 0.6)
    par(mfrow = c(n, n), mgp = c(2, 0.8, 0), oma = rep(3, 4),
        mar = rep(0.5, 4), tck = -0.03/n, err = -1)
    on.exit({
        par(oldpar)
    })
    par(cex = CEX)
    if (length(labels) < n)
        labels <- paste(deparse(substitute(x.1)), "[,", 1:n, "]",
            sep = "")
    if (par("pty") == "s") {
        dif <- diff(par("fin"))/2
        if (dif > 0)
            par(omi = c(dif * n, 0, dif * n, 0) + par("omi"))
        else par(omi = c(0, (-dif) * n, 0, (-dif) * n) + par("omi"))
    }
    order <- if (invert)
        1:n
    else n:1
    rh.eps <- 0.001
    rh.size <- 0.4/n
    for (i in order) {
        for (j in 1:n) {
            plot("zplot", xrange[[j]], xrange[[i]],
                type = "n", axes = F, ...)
            box()
            if (i == 1)
                doaxis(3, par("xaxp"), j%%2 == 0, axis.labels[[j]],
                  0)
            if (i == n)
                doaxis(1, par("xaxp"), j%%2 == 1, axis.labels[[j]],
                  0)
            if (j == 1)
                doaxis(2, par("yaxp"), i%%2 == 0, axis.labels[[i]],
                  90)
            if (j == n)
                doaxis(4, par("yaxp"), i%%2 == 1, axis.labels[[i]],
                  90)
            if (i != j) {
                if (i == 1) {
                  if (any(!right.censored))
                    panel(as.vector(x.1[!right.censored, j]), as.vector(x.1[!right.censored,
                      i]), pch = 16, ...)
                  if (any(right.censored))
                    panel(as.vector(x.1[right.censored, j]), as.vector(x.1[right.censored,
                      i]), pch = 2, cex = 1.2, ...)
                }
                if (j == 1) {
                  if (any(right.censored))
                    arrows(as.vector(x.1[right.censored, j]) -
                      rh.eps, as.vector(x.1[right.censored, i]),
                      x.1[right.censored, j] + rh.eps, as.vector(x.1[right.censored,
                        i]), size = rh.size, open = F, rel = F,
                      ...)
                  if (any(!right.censored))
                    panel(as.vector(x.1[!right.censored, j]), as.vector(x.1[!right.censored,
                      i]), pch = 16, ...)
                }
                if (!(i == 1 || j == 1)) {
                  panel(as.vector(x.1[, j]), as.vector(x.1[, i]),
                    pch = 16, ...)
                }
          } else {
                par(usr = c(0, 1, 0, 1))
                text(0.5, 0.5, labels[i], cex = 1.5 * CEX)
            }
        }
    }
    if (title.option == "full")
        mtext(get.data.title(x), outer = T, line = 2, cex = 1.2)
    invisible()
}
