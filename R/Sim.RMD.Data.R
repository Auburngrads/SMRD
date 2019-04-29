sim.RMD.Data <-
function (object, RMD.plan, plot.stuff = T, print.stuff = T,
    data.title = NULL, degradation.units = "Degradation",...)
{
    time.units <- attr(RMD.plan, "time.units")
    sample.sizes <- RMD.plan$sample.sizes
    time.vectors <- RMD.plan$time.vectors
    if (length(sample.sizes) != length(time.vectors)) {
        print.default(RMD.plan)
        stop("Corrupted RMD.plan object")
    }
    number.of.units <- sum(sample.sizes)
    beta <- rbvn(n = number.of.units, mu1 = object["mu.beta0"],
        mu2 = object["mu.beta1"], sigma1 = object["sigma.beta0"],
        sigma2 = object["sigma.beta1"], rho = object["rho"])
    if (plot.stuff) {
        plot.paper(range(beta[, 1]), range(beta[, 2]), grids = F,
            xlab = "Intercept", ylab = "Slope")
        points(beta[, 1], beta[, 2])
    }
    unique.times <- unlist(RMD.plan$time.vectors)
    DegTrue <- t(beta %*% t(cbind(1, unique.times)))
    if (plot.stuff) {
        plot.paper(range(unique.times), range(DegTrue), grids = F,
            xlab = time.units, ylab = degradation.units)
        title("Simulated Unit Degradation Paths")
        start <- 1
        end <- sample.sizes[1]
        for (i in 1:length(sample.sizes)) {
            if (i > 1) {
                start <- end + 1
                end <- start + sample.sizes[i] - 1
            }
            matlines(unique.times, DegTrue[, start:end], type = "l",
                pch = 1, col = i, lty = 1)
        }
    }
    if (plot.stuff) {
        plot.paper(range(unique.times), range(DegTrue), grids = F,
            xlab = time.units, ylab = degradation.units)
        title(paste("Simulated Data (Degradation Path Plus Measurement Error)\n",
            "Plan:", getPlanString(RMD.plan)))
    }
    Time <- numeric(0)
    Degradation <- numeric(0)
    DiffUnitCount <- numeric(0)
    start <- 1
    end <- sample.sizes[1]
    for (i in 1:length(sample.sizes)) {
        if (i > 1) {
            start <- end + 1
            end <- start + sample.sizes[i] - 1
        }
        sub.number.units <- sample.sizes[i]
        subtime.vec <- RMD.plan$time.vectors[[i]]
        sub.number.inspections <- length(subtime.vec)
        if (print.stuff) {
            cat("Inspection scheme", i, "has", sub.number.units,
                "units inspected at", sub.number.inspections,
                "inspection times\n")
            cat(paste(time.vectors[[i]], sep = ","), "\n\n")
        }
        subDegTrue <- t(beta[start:end, ] %*% t(cbind(1, subtime.vec)))
        subDiffUnitCount <- rep(c(1, rep(0, times = length(subtime.vec) -
            1)), times = sub.number.units)
        subDegMeasured <- subDegTrue + rnorm(sub.number.inspections *
            sub.number.units) * object["sigma.measurement"]
        if (plot.stuff)
            matlines(subtime.vec, subDegMeasured, type = "b",
                pch = 1, col = i, lty = 1)
        Time <- c(Time, rep(subtime.vec, sub.number.units))
        Degradation <- c(Degradation, as.vector(subDegMeasured))
        DiffUnitCount <- c(DiffUnitCount, subDiffUnitCount)
    }
    Unit <- paste("Unit", cumsum(DiffUnitCount), sep = "")
    the.frame <- data.frame(Time = Time, Unit = Unit, Degradation = Degradation)
    if (is.null(data.title))
        data.title <- paste("Simulated Repeated Measures Data",
            deparse(substitute(object)), "\n", "Plan:", deparse(substitute(RMD.plan)),
            getPlanString(RMD.plan))
    attr(the.frame, "RMD.plan") <- RMD.plan
    attr(the.frame, "object") <- object
    attr(the.frame, "data.title") <- data.title
    attr(the.frame, "beta") <- beta
    attr(the.frame, "time.units") <- time.units
    attr(the.frame, "degradation.units") <- degradation.units
    oldClass(the.frame) <- c("rmdnew", "data.frame")
    MysetOldClass(attr(the.frame, "class"))
    invisible(the.frame)
}
