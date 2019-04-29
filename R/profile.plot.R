profile.plot <-
function (fitted, profile.title = paste(fitted$subtitle, "\n",
    lplot.type, "Likelihood and ", percent.conf.level(conf.level),
    "Confidence Interval", "\n for", variable.name, model.dist.str),
    confidence.interval = T, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    xlim = NULL, ylim = c(0, 1.05), log.axis = F, variable.name = fitted$xlab,
    original.par = F, add = F, lty = 1, lwd = 2, print.ci = T,
    col = 1, plotem = T, need.smooth = F, approx.plot = F,...)
{
    digits <- options(digits = GetSMRDDefault("SMRD.DigitsPrinted"))
    on.exit(options(digits))
    model.dist.str <- NULL
    if (is.null(fitted$distribution)) {
        if (is.null(fitted$form)) {
            model.dist.str <- NULL
      } else {
            model.dist.str <- paste("from the", fitted$form,
                " Model")
        }
  } else {
        if (is.null(fitted$form)) {
            model.dist.str <- paste("from the", fitted$distribution,
                "Distribution")
      } else {
            model.dist.str <- paste("from the", fitted$distribution,
                fitted$form, " Model")
        }
    }
    if (plotem && !add) {
        if (is.null(xlim))
            xlim <- range(fitted$x)
        if (log.axis)
            data.axes.out <- logax(xlim[1], xlim[2])
        else data.axes.out <- linax(xlim[1], xlim[2])
        if (approx.plot)
            if.approx.str <- "Approximate"
        else if.approx.str <- ""
        if (!is.null(fitted$number.parameters) && fitted$number.parameters ==
            1)
            lplot.type <- paste(if.approx.str, "Relative")
        else lplot.type <- paste(if.approx.str, "Profile")
        if (profile.title == "")
            top.mar <- 4.1
        else top.mar <- 7
        cex.plot<- 1.1
        cex.lab<- 1.1
        cex.side3<- 1.1
        cex.side4<- 1.1
        if (sum(par("mfrow")) != 2) {
            top.mar <- 3
            cex.plot<- 1.1
            cex.lab<- 1.1
            cex.side3<- 1.1
            cex.side4<- 1.1
        }
        old.par <- par(mar = c(4.5, 5, 3.5, 4) + 0.1, yaxs = "i")
        if (original.par)
            on.exit(par(old.par))
        plot(pp.data(xlim, log.axis), ylim, type = "n",
            xaxt = "n", yaxt = 'n', xlab = "", ylab = "", cex.axis = cex.plot,
            las = 1)
        data.tick.location <- as.numeric(data.axes.out$ticloc)
        data.tick.label.loc <- as.numeric(data.axes.out$ticlab)
        axis(side = 1, at = pp.data(data.tick.location, log.axis),
            labels = F, tck = -0.01, mgp = c(5, 2.1, 0), cex.axis = 1.1)
        axis(side = 1, at = pp.data(data.tick.label.loc, log.axis),
            labels = data.axes.out$ticlab, adj = 0.5, tck = -0.02,
            mgp = c(5, 1.6, 0), cex.axis = cex.plot)
        axis(side = 2, tck = .025, las = 1, hadj = 0.15)
        title(ylab = paste(lplot.type, "Likelihood"), cex.lab = cex.lab, line = 2)
        if (is.null(variable.name)) {
            warning("Null variable name---check gmle caller")
            variable.name <- "variable.name"
        }
        if (is.postsctiptok() && substring(variable.name, 1,
            1) == "~") {
            mixed.mtext(side = 1, line = 2.8, texts = variable.name,
                adj = 0.5, cex = 1 * cex.lab)
      } else {
            title(xlab = parse(text = variable.name), cex.lab = cex.lab)
        }
        if (!is.R()) {
            line.adj <- 0
            spaces <- "     "
      } else {
            line.adj <- -2
            spaces <- ""
        }
        #mtext(profile.title, side = 3, outer = F, line = 4 + line.adj, cex = cex.lab)
        abline(h = 0)
        mtext("Confidence Level", side = 4, outer = F, line = 2,
            cex = cex.lab, las = 0)
        if (confidence.interval) {
            axis.probs <- c(.50, .60, .70, .80, .90, .95, .99)
            axis(side = 4, 
                 at = exp(.Uminus(qchisq(axis.probs,1))/2), 
                 labels = paste(spaces, format(axis.probs)),
                 las = 1, 
                 cex.axis = 1,
                 tck = .025,
                 hadj = .5)
        }
    }
    if (confidence.interval) {
        usr.out <- par("usr")
        hvalue <- exp(.Uminus(qchisq(conf.level, 1))/2)
        ci <- ci.from.profile(fitted, conf.level)
        if (print.ci)
            message("The approximate ", percent.conf.level(conf.level),
                " likelihood confidence interval for ", fitted$xlab,
                " is: ",'(', format(ci[1]),', ', format(ci[2]),')', "\n")
        ci.plot <- pp.data(ci, log.axis)
        if (plotem) {
            abline(h = hvalue)
            if (!is.na(ci.plot[1]))
                plot.line(c(ci.plot[1], usr.out[3]), c(ci.plot[1],
                  hvalue), lty = lty, col = col, lwd = 2)
            if (!is.na(ci[2]))
                plot.line(c(ci.plot[2], usr.out[3]), c(ci.plot[2],
                  hvalue), lty = lty, col = col, lwd = 2)
        }
    }
    if (plotem) {
        lines(pp.data(fitted$x, log.axis), fitted$y, type = "l",
            lty = lty, col = "blue", lwd = lwd)
    }
    if (plotem && !add)
        CheckPrintDataName()
    invisible(ci)
}
