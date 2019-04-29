print.recurrence.data <-
function (x, includex = T, quote = T, prefix = "",...)
{
    event <- events(x)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(x)
    UnitID <- as.factor(get.UnitID(x))
    WindowInfo <- attr(x, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    RecurrTimes <- Times[CriticalEvent, ]
    RecurrUnitID <- UnitID[CriticalEvent]
    RecurrEvents <- event[CriticalEvent]
    RecurrCosts <- get.Costs(x)[CriticalEvent]
    RiskSetCounts <- RiskSet(x)$Counts
    cat(paste("\nRecurrences in", get.data.title(x), "\n"))
    the.frame <- data.frame(cbind(as.character(RecurrUnitID),
        RecurrTimes, as.character(RecurrEvents), RecurrCosts))
    names(the.frame) <- c("Unit", dimnames(Times)[[2]], "Event/Mode",
        "Cost/Count")
    print.data.frame(the.frame)
    if (length(WindowPoint) == length(WindowL)) {
        cat(paste("\nObservation period for units in", get.data.title(x),
            "\n\n"))
        obs.name <- "   Observation period:"
    }
    else {
        cat(paste("\nObservation windows for units in", get.data.title(x),
            "\n\n"))
        obs.name <- "   Observation windows:\n"
    }
    for (i in 1:length(WindowPoint)) {
        unit.names <- names(WindowPoint)
        if (i < length(WindowPoint))
            end.point <- WindowPoint[i + 1] - 1
        else end.point <- length(WindowU)
        the.ones <- WindowPoint[i]:end.point
        windows <- paste("[", WindowL[the.ones], ", ", WindowU[the.ones],
            "]", sep = "")
        cat("Unit", unit.names[i], obs.name, windows, "\n")
    }
    invisible(the.frame)
}
