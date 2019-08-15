ModeCensorBVNData <-
function (BVNData, censor.time, do.ps = F, colors = rep(0, 10))
{
    FieldData <- BVNData$DataSource == "Field"
    WearTime <- BVNData$WearTime[FieldData]
    CrackTime <- BVNData$CrackTime[FieldData]
    maxCrackTime <- max(CrackTime)
    maxWearTime <- max(WearTime)
    number.units <- length(CrackTime)
    DataSource <- rep("Field", number.units)
    Status <- rep("Censored", number.units)
    Weight <- rep(1, number.units)
    Time <- min(WearTime, CrackTime)
    FailureMode <- rep("", number.units)
    WearFailed <- WearTime < CrackTime & WearTime <= censor.time
    CrackFailed <- WearTime > CrackTime & CrackTime <= censor.time
    Censored <- WearTime > censor.time & CrackTime > censor.time

    plot.paper(x = exp(range(WearTime)), y = exp(range(CrackTime)),
        x.axis = "log", y.axis = "log", grids = F, my.title = "",
        ylab = "Crack Time", xlab = "Wear Time")
    points(WearTime[WearFailed], CrackTime[WearFailed], col = 2)
    points(WearTime[CrackFailed], CrackTime[CrackFailed])
    the.title <- paste("Field Data Censored at time", format(exp(censor.time)),
        "\nmaxCrackTime=", format(exp(maxCrackTime)), "maxWearTime=",
        format(exp(maxWearTime)))
    if (!do.ps)
        title(the.title)
    text(x.loc(0.7), y.loc(0.3), "Crack Failures", col = 1)
    text(x.loc(0.4), y.loc(0.7), "Wear Failures", col = 2)
    abline(0, 1)
    if (do.ps)
        dev.off()
    if (any(WearFailed)) {
        Time[WearFailed] <- WearTime[WearFailed]
        FailureMode[WearFailed] <- "Wear"
        Status[WearFailed] <- "Failed"
    }
    if (any(CrackFailed)) {
        Time[CrackFailed] <- CrackTime[CrackFailed]
        FailureMode[CrackFailed] <- "Crack"
        Status[CrackFailed] <- "Failed"
    }
    if (any(Censored)) {
        Time[Censored] <- censor.time
        FailureMode[Censored] <- "Censored"
        Status[Censored] <- "Censored"
    }
    LabData <- BVNData$DataSource == "Lab"
    WearLabTime <- BVNData$WearTime[LabData]
    CrackLabTime <- BVNData$CrackTime[LabData]
    numberLab = length(WearLabTime)
    WearFailureMode = rep("Wear", numberLab)
    CrackFailureMode = rep("Crack", numberLab)
    WearDataSource = rep("TestWear", numberLab)
    CrackDataSource = rep("TestCrack", numberLab)
    first.frame <- data.frame(DataSource = c(DataSource, WearDataSource,
        CrackDataSource), Status = c(Status, rep("Fail", 2 *
        numberLab)), Time = exp(c(Time, WearLabTime, CrackLabTime)),
        FailureMode = c(FailureMode, WearFailureMode, CrackFailureMode))
    return(compress.data.frame(first.frame))
}
