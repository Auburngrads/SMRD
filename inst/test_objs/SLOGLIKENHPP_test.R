data.rdu
theta.mat
form

    if (!is.matrix(theta.mat))
        theta.mat <- as.matrix(theta.mat)
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    WindowInfo <- attr(data.rdu, "WindowInfo")
    event <- data.rdu[, event.column]
    Times <- data.rdu[, time.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    Cevent <- !(EndPoints | StartPoints)
    RecurrCosts <- get.Costs(data.rdu)[Cevent]
    Cevent.times <- Times[Cevent]
    zout <- .Fortran("sloglikenhpp", time = as.double(Cevent.times),
        ntimes = as.integer(length(Cevent.times)), timeL = as.double(WindowInfo$WindowL),
        timeU = as.double(WindowInfo$WindowU), kwcount = as.integer(WindowInfo$WindowCounts),
        nwindows = as.integer(length(WindowInfo$WindowU)), kform = as.integer(num.nhpp.form(generic.nhpp.form(form)[[1]])),
        thetav = as.double(t(theta.mat)), nparm = as.integer(nrow(theta.mat)),
        ntheta = as.integer(ncol(theta.mat)), answer = double(ncol(theta.mat)))
