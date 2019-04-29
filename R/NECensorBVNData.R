NECensorBVNData <-
function (BVNData, censor.time) 
{
    WearTime <- BVNData$WearUseRate
    CrackTime <- BVNData$CrackUseRate
    number.units <- length(censor.time)
    Status <- rep("Failed", number.units)
    Censored <- WearTime > censor.time & CrackTime > censor.time
    if (any(Censored)) {
        WearTime[Censored] <- censor.time
        CrackTime[Censored] <- censor.time
        Status[Censored] <- "Censored"
        Status[!Censored] <- "Failed"
    }
    first.frame <- data.frame(WearTime, CrackTime, Status)
    plot(WearTime, CrackTime)
    return(compress.data.frame(first.frame))
}
