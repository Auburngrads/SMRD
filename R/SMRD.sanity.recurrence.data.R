SMRD.sanity.recurrence.data <-
function (x, check.endpoints.failures = T,...)
{
    event <- events(x)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(x)
    UnitID <- as.factor(get.UnitID(x))
    uniq.UnitID <- unique(UnitID)
    WindowInfo <- attr(x, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    RecurrTimes <- Times[CriticalEvent, ]
    RecurrUnitID <- UnitID[CriticalEvent]
    RecurrEvents <- event[CriticalEvent]
    if (check.endpoints.failures) {
        for (i in 1:length(WindowPoint)) {
            unit.names <- names(WindowPoint)
            if (i < length(WindowPoint))
                end.point <- WindowPoint[i + 1] - 1
            else end.point <- length(WindowU)
            the.ones <- WindowPoint[i]:end.point
            the.WindowL <- WindowL[the.ones]
            the.WindowU <- WindowU[the.ones]
            cur.event.time <- CriticalEvent & UnitID == uniq.UnitID[i]
            current.responses <- Times[cur.event.time, , drop = F]
            event.at.lower.window <- is.element(current.responses,
                the.WindowL)
            if (any(event.at.lower.window))
                current.responses[event.at.lower.window, ] <- current.responses[event.at.lower.window,
                  , drop = F] + 1e-04
            event.at.upper.window <- is.element(current.responses,
                the.WindowU)
            if (any(event.at.upper.window))
                current.responses[event.at.upper.window, ] <- current.responses[event.at.upper.window,
                  , drop = F] - 1e-04
            if (F && i == 3) {
                print(current.responses)
                browser()
            }
            Times[cur.event.time, ] <- current.responses
        }
        times(x) <- Times
    }
    invisible(x)
}
