#'
#'
#'

event.plot <-
function (x,...)
UseMethod("event.plot")

#'
#'

event.plot.life.data <-
  function (x, my.title = NULL, xlab = paste(get.time.units(x)),
            ylab = ifelse(is.null(case.weights(x) ), "Unit", ""),
            title.option = GetSMRDDefault("SMRD.TitleOption"), cex.labs = 1.1, cex.tic.lab = 1.1,
            which.units.to.plot = 1:nrow(x), original.par = T, print.row = T,
            count.on.right = T, suppress.ones = T, failpch = 8, fail.cex = 0.9,
            fail.col = 1, add = F,...)
  {
    
    label.factor <- 1
    label.line <- 0.75
    y <- Response(x)
    number.cases <- nrow(y)
    the.case.weights <- case.weights(x)
    if (is.null(the.case.weights))
      the.case.weights <- rep(1, number.cases)
    the.censor.codes <- censor.codes(x)
    if (is.null(the.censor.codes))
      the.censor.codes <- rep(1, number.cases)
    the.censor.codes <- as.character(the.censor.codes)
    iusys <- as.character(1:number.cases)
    number.unit.to.plot <- length(which.units.to.plot)
    maxx <- 1.05 * max(y[which.units.to.plot, ])
    minx <- 0 - maxx/30
    if (is.null(my.title))
      my.title <- get.data.title(x)
    old.par <- par(mar = c(4.35, 5.1, 0.1, 4.1))
    if (original.par)
      on.exit(par(old.par))
    ylim <- c(0, 1)
    y.axis <- "linear"
    x.axis <- "linear"
    if (!add) {
      GetAxesRange.out <- GetAxesRange("event.plot.setup",
                                        x.axis, xlim = c(0, maxx), xlab, y.axis, ylim,
                                        ylab)
      maxx <- GetAxesRange.out$xlim[2]
      minx <- 0 - maxx/30
      plot(c(minx, maxx), ylim, type = "n", xlab = "", ylab = "",
           yaxt = "n", xaxt = "n", axes = FALSE)
      getxax.out <- linax(0, maxx)
      datax.tick.location <- as.numeric(getxax.out$ticloc)
      datax.tick.label.loc <- as.numeric(getxax.out$ticlab)
      axis(side = 1, at = datax.tick.location, labels = F,
           tck = -0.01, mgp = c(5, 2.1, 0), cex.axis = 1.1)
      xlabels <- vector.power10(getxax.out$ticlab)
      if (is.postsctiptok() && substring(xlabels[1], 1, 1) ==
          "~") {
        mixed.mtext.vec(side = 1, at = datax.tick.label.loc,
                        texts = xlabels, adj = 0.5, cex = cex.labs/label.factor,
                        line = label.line)
        axis(side = 1, at = datax.tick.label.loc, labels = F,
             tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
      } else {
        axis(side = 1, at = datax.tick.label.loc, labels = fix.exp.labels(getxax.out$ticlab),
             adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
      }
      title(xlab = xlab, cex.lab = 1.1)
      mtext(side = 2, "Interval / Failure Event", cex = 1.1, line = 0.75)
      if (title.option == "full")
        title(my.title)
    }
    textx <- par("usr")[1] + 0.1
    ydelta <- 1/(number.unit.to.plot + 1)
    circle.delta <- min(ydelta, 1/20)
    if (map.SMRDDebugLevel() >= 4)
      cat("circle.delta=", circle.delta, "\n")
    vindic <- min(ydelta/3, 1/(11 * 3))
    ypos <- (number.unit.to.plot + 1) * ydelta
    ypos.inc <- 0.25 * ydelta
    horiz.inc <- (x.loc(1) - x.loc(0))/50
    case.weight.dominate <- length(the.case.weights[the.case.weights ==
                                                      1])/length(the.case.weights) > 0.1
    if (number.unit.to.plot > number.cases)
      stop("Number requested for plotting greater than number of units")
    the.ypos <- rep(0, number.cases)
    the.failure.modes <- failure.modes(x)
    if (!is.null(the.failure.modes)) {
      mode.plot.sym <- rep("$", length(the.failure.modes))
      plot.failure.modes <- T
      character.mode <- as.character(the.failure.modes)
      the.failed <- is.onlist(casefold(character.mode), ClistToVec(GetSMRDDefault("SMRD.RcName")))
      if (length(unique(substring(character.mode[the.failed],
                                  1, 1))) == length(unique(character.mode[the.failed]))) {
        mode.plot.sym[the.failed] <- substring(character.mode[the.failed],
                                               1, 1)
      } else {
        factor.mode <- factor(character.mode)
        unique.failure.modes <- unique(factor.mode[the.failed])
        factor.numbers <- as.character(as.numeric(unique.failure.modes))
        order.index <- order(factor.numbers)
        the.table <- data.frame(Number = factor.numbers[order.index],
                                Failure.Mode = unique.failure.modes[order.index])
        print(the.table)
        mode.plot.sym <- as.character(as.numeric(factor.mode))
      }
      xdelta <- 0.01 * (maxx - minx)
    } else {
      plot.failure.modes <- F
      mode.plot.sym <- rep("", length(the.censor.codes))
    }
    for (is in which.units.to.plot) {
      ypos <- ypos - ydelta
      the.ypos[is] <- ypos
      if (print.row)
        text(textx, ypos, paste(" ", iusys[is]), adj = 0, cex = 0.8)
      switch(the.censor.codes[is], `1` = , `5` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lwd = 2)
        if (plot.failure.modes) {
          points.default(y[is, 1] + xdelta, ypos, pch = mode.plot.sym[is],
                         cex = 0.7)
          if (circle.delta > 1/40) {
            if (is.R()) {
              symbols(y[is, 1] + xdelta, ypos, circles = 1,
                      add = T, inches = 3.5 * circle.delta *
                        0.7)
            } else {
              symbols(y[is, 1] + xdelta, ypos, circles = 1,
                      add = T, inches = 3.5 * circle.delta)
            }
          }
        } else {
          points.default(y[is, 1], ypos, pch = failpch,
                         cex = fail.cex, col = fail.col)
        }
      }, `2` = {
        arrows(0, ypos, y[is, 1], ypos, length = 0.1, lwd = 2)
      }, `3` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lty = 3, lwd = 2)
        text(y[is, 1], ypos, "|", adj = 0)
        text(y[is, 1]/2, ypos, paste("?", mode.plot.sym[is],
                                     sep = ""), adj = 0)
      }, `4` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lty = 1, lwd = 2)
        lines(c(y[is, 1], y[is, 2]), c(ypos, ypos), lty = 3,
              lwd = 2)
        text(y[is, 1], ypos, "|", adj = 0)
        text(y[is, 2], ypos, "|", adj = 0)
        if ((y[is, 2] - y[is, 1])/(x.loc(1) - x.loc(0)) >
            0.02) text((y[is, 1] + y[is, 2])/2, ypos, paste("?",
                                                            mode.plot.sym[is], sep = ""), adj = 0)
      }, stop("Unrecognized censor code", the.censor.codes[is],
              "in row", is))
      if (!count.on.right) {
        the.case.weight <- as.character(the.case.weights[is])
        if (the.case.weight == "1")
          the.case.weight <- ""
        text(y[is, ncol(y)] + horiz.inc, ypos + ypos.inc,
             the.case.weight, adj = 0, cex = 0.8)
      }
    }
    if (!all(the.case.weights == 1) && count.on.right) {
      the.case.weights <- as.character(the.case.weights)
      if (suppress.ones && length(the.case.weights[the.case.weights ==
                                                   1])/length(the.case.weights) > 0.1)
        the.case.weights[the.case.weights == 1] <- ""
      
      mtext(the.case.weights[which.units.to.plot], side = 4,
            at = the.ypos[which.units.to.plot], adj = 1,
            line = 1.5, las = 1, cex = 0.8)
      
      mtext(side = 4, at = max(ylim)*1.03, expression(underline(bold("Count"))), 
            line = 1.5, las = 1, cex = 0.8, adj = 1)
      
      #text(x.loc(1.03), y.loc(1.04), expression(underline(bold("Count"))))
    }
    if (print.row)
      #text(x.loc(0.01), y.loc(0.97), expression(underline(bold("Row"))), adj = 0)
      CheckPrintDataName()
    invisible()
  }

#'
#'

event.plot.recurrence.data <-
  function (x, my.title = NULL, xlab = paste("Time in",
                                             get.time.units(x)), ylab = "System ID", title.option = GetSMRDDefault("SMRD.TitleOption"),
            cex.labs = 1.1, cex.tic.lab = 1.1, which.system.to.plot = 1:nsys,
            cex.text = NULL, xlim = c(NA, NA), sort.id = F, maxx = NA,...)
  {
    label.factor <- 1
    old.par <- par(mar = c(4.5, 5.1, 0.5, 2.1))
    on.exit({
      par(old.par)
      par(new = F)
    })
    WindowInfo <- attr(x, "WindowInfo")
    event <- events(x)
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(x)
    UnitID <- get.UnitID(x)
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    the.case.weights <- WindowInfo$WindowCounts
    MEndPoints <- is.element(casefold(event), c("mend"))
    label.line <- 0.75
    UniqueUnitID <- as.character(WindowInfo$IDOrdered)
    UniqueUnitIDOrigOrder <- as.character(WindowInfo$IDOrigOrder)
    nsys <- length(UniqueUnitID)
    if (!sort.id) {
      which.system.to.plot <- match(UniqueUnitIDOrigOrder,
                                    UniqueUnitID)[which.system.to.plot]
    }
    number.system.to.plot <- length(which.system.to.plot)
    if (is.na(maxx))
      maxx <- max(WindowInfo$WindowU)
    if (is.null(my.title)) {
      my.title <- get.data.title(x)
    }
    ylim <- c(0, 1)
    y.axis <- "linear"
    x.axis <- "linear"
    GetAxesRange.out <- GetAxesRange("event.plot.setup", x.axis,
                                      xlim = c(0, maxx), xlab, y.axis, ylim, ylab)
    maxx <- GetAxesRange.out$xlim[2]
    minx <- 0 - maxx/30
    plot(c(minx, maxx), ylim, type = "n", xlab = "", ylab = "",
         yaxt = "n", xaxt = "n", bty = "n")
    getxax.out <- linax(0, maxx)
    datax.tick.location <- as.numeric(getxax.out$ticloc)
    datax.tick.label.loc <- as.numeric(getxax.out$ticlab)
    axis(side = 1, at = datax.tick.location, labels = F, tck = -0.01,
         mgp = c(5, 2.1, 0), cex = 1)
    xlabels <- vector.power10(getxax.out$ticlab)
    if (is.postscriptok() && substring(xlabels[1], 1, 1) == "~") {
      mixed.mtext.vec(side = 1, at = datax.tick.label.loc,
                      texts = xlabels, adj = 0.5, cex = cex.labs/label.factor,
                      line = label.line)
      axis(side = 1, at = datax.tick.label.loc, labels = F,
           tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
    } else {
      axis(side = 1, at = datax.tick.label.loc, labels = fix.exp.labels(getxax.out$ticlab),
           adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
    }
    title(xlab = xlab, ylab = ylab, cex.lab = 1.1)
    if (title.option == "full")
      title(my.title)
    textx <- par("usr")[1]
    ydelta <- 1/(number.system.to.plot + 1)
    if (is.null(cex.text)) {
      if (number.system.to.plot < 20)
        cex.text <- 1
      else if (number.system.to.plot > 41)
        cex.text <- 0.75
      else {
        f <- function(x) {
          ((x - 20)/(41 - 20)) * 0.75 + ((41 - x)/(41 -
                                                     20)) * 1
        }
        cex.text <- f(number.system.to.plot)
      }
    }
    vindic <- min(ydelta/3, 1/(11 * 3))
    ypos <- (number.system.to.plot + 1) * ydelta
    if (number.system.to.plot > nsys)
      stop("Number requested for plotting greater than number of systems")
    the.ypos <- rep(0, nsys)
    for (is in which.system.to.plot) {
      ypos <- ypos - ydelta
      the.ypos[is] <- ypos
      axis(side = 2, at = ypos, labels = paste(" ", UniqueUnitID[is]), adj = 0,
           cex.axis = cex.tic.lab-(number.system.to.plot/200), las = 1, tck = 0, hadj = 0.25)
      #text(textx, ypos, paste(" ", UniqueUnitID[is]), adj = 0,
      #    cex = cex.text)
      Wstart <- WindowPoint[is]
      if (is < nsys)
        Wend <- WindowPoint[is + 1] - 1
      else Wend <- length(WindowU)
      for (i in Wstart:Wend) {
        DrawLine(c(WindowL[i], ypos), c(WindowU[i], ypos),
                 lwd = 2, col = "blue")
        if (WindowL[i] > 0)
          DrawLine(c(WindowL[i], ypos - vindic), c(WindowL[i],
                                                   ypos + vindic), lwd = 2, col = "darkblue")
        DrawLine(c(WindowU[i], ypos - vindic), c(WindowU[i],
                                                 ypos + vindic), lwd = 2, col = "darkblue")
      }
      event.times <- Times[CriticalEvent & UniqueUnitID[is] ==
                             UnitID]
      points.default(event.times, rep(ypos, length = length(event.times)),
                     pch = 4, lwd = 2, cex = 1.15)
    }
    if (any(MEndPoints) && (length(the.case.weights) == nsys)) {
      weights.to.plot <- is.element(which.system.to.plot, 1:nsys) &
        the.case.weights > 1
      if (any(weights.to.plot)) {
        text(x.loc(1.03), y.loc(1.04), "Count")
        
        mtext(as.character(the.case.weights[weights.to.plot]),
              side = 4, at = the.ypos[weights.to.plot], adj = 1,
              line = 1.8, las = 1)
      }
    }
    CheckPrintDataName()
    invisible()
  }
