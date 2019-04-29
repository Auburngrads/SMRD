summary.recurrence.data <-
function (object,...)
{
    event <- events(object)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "remov\ned"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(object)
    UnitID <- as.factor(get.UnitID(object))
    WindowInfo <- attr(object, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    RecurrTimes <- Times[CriticalEvent, ]
    RecurrUnitID <- UnitID[CriticalEvent]
    RecurrCosts <- get.Costs(object)[CriticalEvent]
    RiskSetCounts <- RiskSet(object)$Counts
    the.data.note <- get.data.note(object)
    cat(paste("\nSummary of  ", get.data.title(object), "data\n"))
    if (length(the.data.note) > 0 && the.data.note != "")
        cat(paste("\n", parse.note(the.data.note), "\n", sep = ""))
    cat(paste("\nNumber of rows in data matrix=", nrow(Times),
        "\n"))
    cat(paste("Number of unique units or group IDs in the recurrence data object =",
        length(unique(UnitID)), "\n"))
    cat(paste("Number of observation windows =", length(attr(object,
        "WindowInfo")$WindowL), "\n"))
    attr(object, "WindowInfo")$WindowL
    number.recurrences <- length(RecurrTimes)
    if (number.recurrences > 0) {
        cat(paste("Number of recurrences =", number.recurrences,
            "\n"))
    }
    cat(paste("Sum of costs/counts: ", sum(RecurrCosts), "\n"))
    number.recurrences <- length(unique(RecurrTimes))
    if (number.recurrences > 0) {
        cat(paste("Number of unique recurrence times =", number.recurrences,
            "\n"))
    }
    if (map.SMRDDebugLevel() >= 4) {
        print(sort(unique(RecurrTimes)))
    }
    if (map.SMRDDebugLevel() >= 6) {
        print(sort(unique(UnitID)))
    }
    cat(paste("Time units: ", get.time.units(object), "\n"))
    cat(paste("Recurrence time minimum: ", min(RecurrTimes),
        get.time.units(object), "\n"))
    cat(paste("Recurrence time maximum: ", max(RecurrTimes),
        get.time.units(object), "\n"))
    cat(paste("Endpoint time maximum: ", max(Times), get.time.units(object),
        "\n\n"))
    invisible()
}
