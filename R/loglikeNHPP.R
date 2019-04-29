loglikeNHPP <-
function (data.rdu, theta, form)
{
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    event <- data.rdu[, event.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    WindowInfo <- attr(data.rdu, "WindowInfo")
    Times <- data.rdu[, time.column]
    Cevent <- !(EndPoints | StartPoints)
    RecurrCosts <- get.Costs(data.rdu)[Cevent]
    Cevent.times <- Times[Cevent]
    zout <- .Fortran("xloglikenhpp", time = as.double(Cevent.times),
        ntimes = as.integer(length(Cevent.times)), RecurrCosts = as.double(RecurrCosts),
        timeL = as.double(WindowInfo$WindowL), timeU = as.double(WindowInfo$WindowU),
        kwcount = as.integer(WindowInfo$WindowCounts), nwindows = as.integer(length(WindowInfo$WindowU)),
        kform = as.integer(num.nhpp.form(generic.nhpp.form(form)[[1]])),
        theta = as.double(theta), answer = double(1))
    return(zout$answer)
}
