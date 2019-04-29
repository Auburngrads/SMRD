conf.contour <-
function (struct1, xlim = c(NA, NA), ylim = c(NA, NA),
    profile.title = paste(struct1$subtitle, "\n", "Joint Confidence Regions",
        "for", variable.namex, "and", variable.namey, "\n", model.dist.str),
    variable.namex = struct1$xlab, variable.namey = struct1$ylab,
    transformationx = "linear", transformationy = "linear", original.par = T,
    levels = c(99, 95, 90, 80, 70, 50), pretty.x = NULL, pretty.y = NULL,
    add = F, lty = 1, col = 1, lwd = 1,...)
{
    if (!is.null(struct1$number.parameters) && struct1$number.parameters ==
        2)
        lplot.type <- "Relative"
    else lplot.type <- "Profile"
    model.dist.str <- NULL
    if (is.null(struct1$distribution)) {
        if (is.null(struct1$form)) {
            model.dist.str <- NULL
      } else {
            model.dist.str <- paste("from the", struct1$form,
                " Model")
        }
  } else {
        if (is.null(struct1$form)) {
            model.dist.str <- paste("from the", struct1$distribution,
                "Distribution")
      } else {
            model.dist.str <- paste("from the", struct1$distribution,
                struct1$form, " Model")
        }
    }
    if (!add) {
        par(mar = c(4.5, 5, 3.5, 2) + 0.1)
        old.par <- par(mar = c(4.5, 5, 3.5, 2) + 0.1, cex = 1.1)
        if (original.par)
            on.exit(par(old.par))
    }
    cex.lab<- 1.1
    if (!add) {
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- range(struct1$x)[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- range(struct1$y)[yrna]
        #plot(xlim, ylim, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    }
    struct1$z <- matrix(100 * pchisq(-2 * logb(struct1$z), 2),
        ncol = ncol(struct1$z))
    image2D(z = struct1$z, x = struct1$x, y = struct1$y, levels = levels,
        xlab = parse(text = variable.namex), ylab = parse(text = variable.namey), 
        lty = lty, cex.lab = cex.lab, lwd = lwd,
        contour = TRUE, clab = c(expression(bold(1-alpha))),...)
    if (!add) {
        
        if (is.null(pretty.x)) {
            if (transformationx == "log") {
                trans.range <- f.relationshipinv(xlim, transformationx)
                pretty.x <- logax(trans.range[1], trans.range[2])$ticlab
          } else {
                pretty.x <- pretty.check(wqm.pretty(f.relationshipinv(xlim,
                  transformationx), nint = 6), transformationx)
            }
        }
        #axis(side = 1, at = f.relationship(as.numeric(pretty.x),
        #    transformationx), labels = format(pretty.x), cex.axis = 1.1, tck = -0.01, 
        #    line = -3.75)
        if (is.null(pretty.y))
            pretty.y <- pretty.check(wqm.pretty(f.relationshipinv(ylim,
                transformationy), nint = 6), transformationy)
        #axis(side = 2, at = f.relationship(as.numeric(pretty.y),
        #    transformationy), labels = format(pretty.y),
        #    adj = 1, tck = -0.01, las = 1, line = -4.2, cex.axis = 1.1)
       
            line.adj <- -2
      
        #mtext(profile.title, side = 3, outer = F, line = 4 + line.adj, cex = 1.1)
    }
    if (!add)
        CheckPrintDataName()
    invisible()
}
