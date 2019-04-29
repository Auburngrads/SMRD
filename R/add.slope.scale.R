add.slope.scale <-
function (distribution, shape, draw.line = F)
{
    usr.out <- par("usr")
    par(xpd = T)
    if (usr.out[4] > 0 && usr.out[3] < 0)
        abline(h = 0, lwd = 2, lty = 2)
    mark.point <- c(usr.out[1] + 0.37 * (usr.out[2] - usr.out[1]),
        usr.out[3] + 1.09 * (usr.out[4] - usr.out[3]))
    points.default(mark.point[1], mark.point[2], cex = 2, pch = 10)
    slope.x.pos <- usr.out[1] - 0.1 * (usr.out[2] - usr.out[1])
    if (is.numeric(draw.line)) {
        y2 <- mark.point[2] + (slope.x.pos - mark.point[1]) *
            draw.line
        lines(c(mark.point[1], slope.x.pos), c(mark.point[2],
            y2))
    }
    slope.lab.pos <- usr.out[1] - 0.12 * (usr.out[2] - usr.out[1])
    slope.range <- c((mark.point[2] - usr.out[4])/(mark.point[1] -
        slope.x.pos), (mark.point[2] - usr.out[3])/(mark.point[1] -
        slope.x.pos))
    slope.tran <- function(x, slope.range, usr.out) {
        return((usr.out[4] - ((as.numeric(x) - slope.range[1])/(slope.range[2] -
            slope.range[1])) * (usr.out[4] - usr.out[3])))
    }
    if (numdist(distribution) == 2 && is.null(shape) || numdist(distribution) ==
        10) {
        slope.x.axis.markers <- linax(slope.range)
        ticks.at <- slope.tran(slope.x.axis.markers$ticloc, slope.range,
            usr.out)
        tick.labs.at <- slope.tran(slope.x.axis.markers$ticlab,
            slope.range, usr.out)
        mixed.label <- "~f13~.b~"
        text.label <- "beta"
  } else {
        slope.x.axis.markers <- linax(sort(1/slope.range))
        ticks.at <- slope.tran(1/as.numeric(slope.x.axis.markers$ticloc),
            slope.range, usr.out)
        tick.labs.at <- slope.tran(1/as.numeric(slope.x.axis.markers$ticlab),
            slope.range, usr.out)
        if (!is.null(shape))
            shape <- "S"
        switch(paste(shape, numdist(distribution), sep = ""),
            S12 = , `14` = {
                mixed.label <- "~f13~.q~"
                text.label <- "theta"
            }, S2 = {
                mixed.label <- "~f13~.h~"
                text.label <- "eta"
            }, S4 = {
                mixed.label <- "~.e~u.8~f13~.m~"
                text.label <- "exp(mu)"
            }, {
                mixed.label <- "~f13~.s~"
                text.label <- "sigma"
            })
        ticlab <- slope.x.axis.markers$ticlab
    }
    if (is.postsctiptok()) {
        mixed.text(slope.lab.pos, usr.out[3] + 1.03 * (usr.out[4] -
            usr.out[3]), mixed.label)
  } else {
        text(slope.lab.pos, usr.out[3] + 1.03 * (usr.out[4] -
            usr.out[3]), text.label)
    }
    text((slope.x.pos + usr.out[1])/2, usr.out[3] + 1.03 * (usr.out[4] -
        usr.out[3]), "F(t)")
    ticks.at <- ticks.at[ticks.at != -Inf]
    ticlab <- slope.x.axis.markers$ticlab[tick.labs.at != -Inf]
    tick.labs.at <- tick.labs.at[tick.labs.at != -Inf]
    axis(side = 2, pos = slope.x.pos, at = ticks.at, labels = F,
        tck = -0.01, mgp = c(5, 2.1, 0), cex = 1)
    axis(side = 2, pos = slope.x.pos, at = tick.labs.at, labels = ticlab,
        adj = 1, tck = -0.02, mgp = c(5, 1.1, 0), cex = 1)
    par(xpd = F)
}
