plot.prob.cs.type2 <-
function (x, k, r, n, d.range = c(NA, NA), prob.range = c(NA,
    NA), d.length = 100, dvec = NULL, number.sim = 500, kprint = 0,
    largest = T, do.legend = T, grids = T,...)
{
    drna <- is.na(d.range)
    if (is.R())
        lty.map <- c(1, 2, 4, 5, 6, 7, 8, 9)
    else lty.map <- c(1, 3, 4, 5, 6, 7, 8, 9)
    if (any(drna))
        d.range[drna] <- c(0.001, 2)[drna]
    prna <- is.na(prob.range)
    if (any(prna))
        prob.range[prna] <- c(1/k, 1)[prna]
    if (is.null(dvec))
        dvec <- seq(d.range[1], d.range[2], length = d.length)
    plot.paper(d.range, prob.range, xlab = "Standardized Difference (d)      ",
        ylab = "Pr(Correct Selection)", grids = grids)
    if (largest) {
        title(paste("Probability of Correctly Selecting the Largest\n",
            dist.info(x)$formal.name, "Population out of",
            k, "Populations"))
}   else {
        title(paste("Probability of Correctly Selecting the Smallest\n",
            x, "x with", k, "Populations"))
        stop("Smallest not yet implemented")
    }
    plot.em <- 1:(length(dvec) - 0.05 * length(dvec))
    if (missing(r) && missing(n)) {
        number.lines <- 0
        repeat {
            cat("Input n,r; 0,0 to stop:  ")
            the.line <- readline()
            if (the.line == "") {
                cat("\nNull input; must enter two numbers \n")
                next
            }
            n.and.r <- as.numeric(unlist(wqm.unpaste(the.line,
                sep = ",")))
            if (any(is.na(n.and.r))) {
                cat("\nIncorrect character data detected or numbers not separated by a comma \n")
                next
            }
            if (length(n.and.r) != 2) {
                cat("Something other than 2 numbers detected \n")
                next
            }
            n <- n.and.r[1]
            r <- n.and.r[2]
            if (r > n) {
                cat("n must be greater than r \n")
                next
            }
            if (n.and.r[2] == 0) {
                cat("Done\n")
                break
            }
            if (n.and.r[2] < 1) {
                cat("n must be greater than 1 \n")
                next
            }
            if (r <= 2) {
                cat("Warning: r=2 can cause etimation problems \n")
            }
            answer <- prob.correct.select.type2(x,
                n = n, r = r, k = k, number.sim = number.sim,
                kprint = kprint)
            number.lines <- number.lines + 1
            lines(answer$dvec[plot.em], answer$prob[plot.em],
                lwd = 2)
            plot.pos <- length(dvec)
            text(answer$dvec[plot.pos], answer$prob[plot.pos],
                paste(n, ",", r, sep = ""))
        }
}   else {
        if (missing(r) != missing(n))
            stop("Must give both n and r if either is given.")
        if (length(n) != length(r))
            stop("n and r must have the same length.")
        text.vec <- rep(NA, length(n))
        number.lines <- length(n)
        for (i in 1:number.lines) {
            text.vec[i] <- paste(paste("n=", n[i], sep = ""),
                paste("r=", r[i], sep = ""), sep = ", ")
            answer <- prob.correct.select.type2(x,
                n = n[i], r = r[i], k = k, number.sim = number.sim,
                kprint = kprint)
            lines(answer$dvec, answer$prob, lty = i, col = i,
                lwd = 2)
            if (i == 2)
                lines(answer$dvec, answer$prob, lty = i, col = i,
                  lwd = 3)
        }
    }
    if (do.legend) {
        bty = "o"
        bg0 = "white"
        lwd.fix <- rep(1, number.lines)
        if (number.lines > 1)
            lwd.fix[2] <- 3
        legend(x.loc(0.6), y.loc(0.4), text.vec, cex = 1.1, bty = bty,
            bg = bg0, col = 1:number.lines, lty = 1:number.lines,
            lwd = lwd.fix, xpd = TRUE, y.intersp = 0.675)
    }
}
