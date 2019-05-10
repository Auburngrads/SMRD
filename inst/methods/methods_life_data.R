life_data_methods <- function(){
  
 setMethod("plot", 
          signature = "life_data",
          definition = function(x,
                                distribution = NULL,
                                events = F,
                                my.title = NULL, 
                                xlab = paste(SMRD2:::get.time.units(x@frame)),
                                ylab = `if`(is.null(SMRD2:::case.weights(x@frame)), "Unit", ""),
                                title.option = SMRD2:::GetSMRDDefault("SMRD.TitleOption"), 
                                cex.labs = 1.1, 
                                cex.tic.lab = 1.1,
                                which.units.to.plot = 1:nrow(x@frame), 
                                original.par = T, 
                                print.row = T,
                                count.on.right = T, 
                                suppress.ones = T, 
                                failpch = 8, 
                                fail.cex = 0.9,
                                fail.col = 1, 
                                add = F,...){
            
if(!events) {
  
   if(is.null(distribution)) SMRD2:::cdfplot(data.ld = x@frame,...)
  
   dist_length = length(distribution)
   
   if(dist_length == 3 | dist_length > 4) stop('Number of distributions must be 1, 2, or 4')

   if(dist_length == 4) SMRD2:::four.npprobplot(data.ld = x@frame, distribution.list = distribution,...)
      
   if(dist_length == 2) SMRD2:::two.npprobplot(data.ld = x@frame, distribution.list = distribution,...)

   if(dist_length == 1) SMRD2:::npprobplot(data.ld = x@frame, distribution = distribution,...)
       
} else {

    label.factor <- 1
    label.line <- 0.75
    y <- SMRD2:::Response(x@frame)
    number.cases <- nrow(y)
    the.case.weights <- SMRD2:::case.weights(x@frame)
    if (is.null(the.case.weights)) the.case.weights <- rep(1, number.cases)
    the.censor.codes <- SMRD2:::censor.codes(x@frame)
    if (is.null(the.censor.codes))the.censor.codes <- rep(1, number.cases)
    the.censor.codes <- as.character(the.censor.codes)
    iusys <- as.character(1:number.cases)
    number.unit.to.plot <- length(which.units.to.plot)
    maxx <- 1.05 * max(y[which.units.to.plot, ])
    minx <- 0 - maxx/30
    if (is.null(my.title))my.title <- SMRD2:::get.data.title(x@frame)
    old.par <- par(mar = c(4.35, 5.1, 0.1, 4.1))
    if (original.par) on.exit(par(old.par))
    ylim <- c(0, 1)
    y.axis <- "linear"
    x.axis <- "linear"
    if (!add) {
      GetAxesRange.out <- SMRD2:::GetAxesRange("event.plot.setup",
                                       x.axis, 
                                       xlim = c(0, maxx), 
                                       xlab, 
                                       y.axis, 
                                       ylim,
                                       ylab)
      maxx <- GetAxesRange.out$xlim[2]
      minx <- 0 - maxx/30
      
      plot(c(minx, maxx), 
           ylim, 
           type = "n", 
           xlab = "", 
           ylab = "",
           yaxt = "n", 
           xaxt = "n", 
           axes = FALSE)
      
      getxax.out <- SMRD2:::linax(0, maxx)
      datax.tick.location <- as.numeric(getxax.out$ticloc)
      datax.tick.label.loc <- as.numeric(getxax.out$ticlab)
      axis(side = 1, at = datax.tick.location, labels = F,
           tck = -0.01, mgp = c(5, 2.1, 0), cex.axis = 1.1)
      xlabels <- SMRD2:::vector.power10(getxax.out$ticlab)
      if (SMRD2:::is.postsctiptok() && substring(xlabels[1], 1, 1) ==
          "~") {
        SMRD2:::mixed.mtext.vec(side = 1, at = datax.tick.label.loc,
                        texts = xlabels, adj = 0.5, cex = cex.labs/label.factor,
                        line = label.line)
        axis(side = 1, at = datax.tick.label.loc, labels = F,
             tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
      } else {
        axis(side = 1, at = datax.tick.label.loc, labels = SMRD2:::fix.exp.labels(getxax.out$ticlab),
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
    if (SMRD2:::map.SMRDDebugLevel() >= 4) cat("circle.delta=", circle.delta, "\n")
    vindic <- min(ydelta/3, 1/(11 * 3))
    ypos <- (number.unit.to.plot + 1) * ydelta
    ypos.inc <- 0.25 * ydelta
    horiz.inc <- (SMRD2:::x.loc(1) - SMRD2:::x.loc(0))/50
    case.weight.dominate <- length(the.case.weights[the.case.weights ==
                                                      1])/length(the.case.weights) > 0.1
    if (number.unit.to.plot > number.cases)
      stop("Number requested for plotting greater than number of units")
    the.ypos <- rep(0, number.cases)
    the.failure.modes <- SMRD2:::failure.modes(x@frame)
    if (!is.null(the.failure.modes)) {
      mode.plot.sym <- rep("$", length(the.failure.modes))
      plot.failure.modes <- T
      character.mode <- as.character(the.failure.modes)
      the.failed <- SMRD2:::is.onlist(casefold(character.mode), SMRD2:::ClistToVec(GetSMRDDefault("SMRD.RcName")))
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
        if ((y[is, 2] - y[is, 1])/(SMRD2:::x.loc(1) - SMRD2:::x.loc(0)) >
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
      SMRD2:::CheckPrintDataName()
    invisible()

}
            
})
  
}
