CumulativePredictTable <-
function (mlest.out, time.increment, number.time.units.ahead, 
    warranty.time) 
{
    number.intervals <- number.time.units.ahead/time.increment
    FtimeEnd = 0
    Efail <- rep(NA, number.intervals)
    Time <- rep(NA, number.intervals)
    for (i in 1:number.intervals) {
        FtimeStart = FtimeEnd
        FtimeEnd = FtimeStart + time.increment
        the.table <- PredictTable(mlest.out, FtimeStart = FtimeStart, 
            FtimeEnd = FtimeEnd, warranty.time)
        Time[i] <- FtimeEnd
        Efail[i] <- sum(the.table$Efail)
    }
    result <- cbind(Time = Time, Efail = Efail)
    attr(result, "mlest.out") <- mlest.out
    result
}
