repair.tsplot <-
function (data.rdu, my.title = NULL, xlab = "Failure Number",
    ylab = "Time Between Failure", ...)
{
    par(mar = c(5.1, 6.1, 4.1, 2.1))
    event <- events(data.rdu)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(data.rdu)
    UnitID <- as.factor(get.UnitID(data.rdu))
    WindowInfo <- attr(data.rdu, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    RecurrTimes <- Times[CriticalEvent, ]
    interarrival.times <- diff(c(0, RecurrTimes))
    if (is.R())
        plot(interarrival.times, xlab = "", ylab = "", pch = 20,
            cex = 1.5, type = "b", las = 1, ...)
    else ts.plot(interarrival.times, xlab = "", ylab = "", cex = 1.5,
        las = 1, ...)
    title(xlab = xlab, cex = 1.5)
    title(ylab = ylab, cex = 1.5, mgp = c(4, 1, 0))
    if (is.null(my.title))
        my.title <- get.data.title(data.rdu)
    title(my.title)
}
