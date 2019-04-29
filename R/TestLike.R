TestLike <-
function (data.rdu, theta, form) 
{
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    WindowInfo <- attr(data.rdu, "WindowInfo")
    event <- data.rdu[, event.column]
    Times <- data.rdu[, time.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    Cevent <- !(EndPoints | StartPoints)
    Cevent.times <- Times[Cevent]
    RecurrCosts <- get.Costs(data.rdu)[Cevent]
    Cevent.counts <- Times[Cevent]
    answer <- Sxloglikenhpp(Cevent.times, RecurrCosts, WindowInfo$WindowL, 
        WindowInfo$WindowU, WindowInfo$WindowCounts, form, theta)
    return(answer)
}
