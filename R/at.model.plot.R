at.model.plot <-
function (x.axis, y.axis, ylim, xlim, my.title, title.option,
    censor.time = NULL, density.scale = NULL, quant.lines.list = NULL,
    density.functions.list = NULL, cex = 1.1, xlab, ylab, lwd = 2,
    grids = F, response.on.yaxis = T, x.data, y.data, the.censor.codes = NULL,
    plot.quant.labels = T, character.label.list, density.anchor.list,
    hw.xaxis = NULL, hw.yaxis = NULL, ...)
{
    if (map.SMRDDebugLevel() >= 5)
        cat("In at.model.plot x=", paste(x.axis, xlim[1],
            xlim[2], sep = ","), paste("y=", y.axis, ylim[1],
            ylim[2], sep = ","), "\n")
    if (missing(character.label.list)) {
        plot.paper(xlim, ylim, x.axis = x.axis, y.axis = y.axis,
            xlab = xlab, ylab = ylab, response.on.yaxis = response.on.yaxis,
            cex = cex, my.title = "", grids = grids, title.option = title.option,
            hw.xaxis = hw.xaxis, hw.yaxis = hw.yaxis, ...)
  } else {
        plot.paper(x = xlim, y = ylim, x.axis = "blank",
            y.axis = y.axis, xlab = xlab, ylab = ylab, response.on.yaxis = response.on.yaxis,
            cex = cex, my.title = "", grids = grids, hw.xaxis = hw.xaxis,
            hw.yaxis = hw.yaxis, ...)
        if (response.on.yaxis)
            axis(side = 1, at = character.label.list$at, labels = character.label.list$labels,
                adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex = cex)
        else axis(side = 2, at = character.label.list$at, labels = character.label.list$labels,
            adj = 0.5, tck = -0.02, mgp = c(5, 4.5, 0), cex = cex)
    }
    if (!is.null(censor.time) && !any(is.na(censor.time))) {
        if (response.on.yaxis)
            abline(h = f.relationship(censor.time, y.axis), col = 6,
                lwd = 3)
        else abline(v = f.relationship(censor.time, y.axis),
            col = 6, lwd = 3)
    }
    if (!missing(density.functions.list) && length(density.functions.list) >
        0) {
        if (is.null(density.scale)) {
            xden.range <- range(density.functions.list[[1]]$xden)
            if (response.on.yaxis)
                density.scale <- (x.loc(1) - x.loc(0))/((xden.range[2] -
                  xden.range[1]) * 20)
            else density.scale <- (y.loc(1) - y.loc(0))/((xden.range[2] -
                xden.range[1]) * 20)
        }
        for (i in 1:length(density.functions.list)) {
            dennow <- density.functions.list[[i]]
            dennow$yden <- f.relationship(dennow$yden, y.axis)
            if (response.on.yaxis) {
                lines(dennow$at + density.scale * dennow$xden,
                  dennow$yden, col = 2, lwd = 2)
                lines(c(dennow$at, dennow$at), range(dennow$yden),
                  lwd = 2)
          } else {
                lines(dennow$yden, dennow$at - density.scale *
                  dennow$xden, col = 2, lwd = 2)
                lines(range(dennow$yden), c(dennow$at, dennow$at),
                  lwd = 2)
            }
        }
    }
    if (!missing(quant.lines.list) && length(quant.lines.list) >
        0) {
        quant.lines.x <- quant.lines.list$quant.lines.x
        quant.lines.y <- quant.lines.list$quant.lines.y
        quant.lines.level <- quant.lines.list$quant.lines.level
        nxpoints <- length(quant.lines.x)
        if (length(quant.lines.level) > 0) {
            for (i in 1:ncol(quant.lines.y)) {
                if (response.on.yaxis) {
                  lines(f.relationship(quant.lines.x, x.axis),
                    f.relationship(quant.lines.y[, i], y.axis),
                    lwd = lwd)
                  if (plot.quant.labels)
#                     text(f.relationship(quant.lines.x[nxpoints],
#                       x.axis), f.relationship(quant.lines.y[nxpoints,
#                       i], y.axis), paste("  ", 100 * quant.lines.level[i],
#                       "%", sep = ""), adj = -0.5, xpd = TRUE)
                    mtext(side = 4, at = f.relationship(quant.lines.y[nxpoints,
                        i], y.axis), text = paste(100 * quant.lines.level[i],
                        "%", sep = ""), adj = -0.15, cex = 1.05, las = 1)
              } else {
                  lines(f.relationship(quant.lines.y[, i], y.axis),
                    f.relationship(quant.lines.x, x.axis), lwd = lwd)
                  if (plot.quant.labels)
#                     text(f.relationship(quant.lines.y[1, i],
#                       y.axis), y.loc(0.07), paste(100 * quant.lines.level[i],
#                       "%", sep = ""), adj = 0.5, xpd = TRUE)
                    mtext(side = 3, at = f.relationship(quant.lines.y[nxpoints, i],
                        y.axis), text = paste(100 * quant.lines.level[i],
                        "%", sep = ""), adj = 0.25, cex = 1.05)
                }
            }
        }
    }
    if (!missing(density.anchor.list) && length(density.anchor.list) >
        0) {
        density.anchor.x <- density.anchor.list$density.anchor.x
        density.anchor.y <- density.anchor.list$density.anchor.y
        for (i in 1:length(density.functions.list)) {
            density.anchor.x <- density.anchor.list[[i]]$density.anchor.x
            density.anchor.y <- density.anchor.list[[i]]$density.anchor.y
            if (response.on.yaxis) {
                lines(f.relationship(density.anchor.x, x.axis),
                  f.relationship(density.anchor.y, y.axis), type = "p",
                  pch = 1, cex = 2)
          } else {
                lines(f.relationship(density.anchor.y, y.axis),
                  f.relationship(density.anchor.x, x.axis), type = "p",
                  pch = 1, cex = 2)
            }
        }
    }
    if (!missing(x.data) && !is.null(x.data) && !missing(y.data) &&
        !is.null(y.data)) {
        plot.censored.data.points(f.relationship(x.data, x.axis),
            f.relationship(y.data, y.axis), the.censor.codes,
            x.axis = x.axis, y.axis = y.axis, response.on.yaxis = response.on.yaxis,
            cex = cex)
    }
    if (is.R())
        title.plot.line <- 0.5
    else title.plot.line <- 4
    if (title.option == "full")
        mtext(text = my.title, side = 3, cex = 1.2, line = title.plot.line)
    invisible()
}
