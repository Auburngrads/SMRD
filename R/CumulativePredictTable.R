#' Title
#'
#' @param mlest.out 
#' @param time.increment 
#' @param number.time.units.ahead 
#' @param warranty.time 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # No warranty enforcement
#' 
#' DeviceN.NoEnforce.weib <-
#'   CumulativePredictTable(DeviceN.mlest.weib,
#'                          time.increment = 1,
#'                          number.time.units.ahead = 50,
#'                          warranty.time = 1000)
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "density")
#' 
#' PlotCumulativePredictTable(DeviceN.NoEnforce.weib,
#'                            plot.what = "cumulative")
#' 
#' # Strict warranty enforcement
#' 
#' DeviceN.Enforced.weib <-
#'   CumulativePredictTable(DeviceN.mlest.weib,
#'                          time.increment = 1,
#'                          number.time.units.ahead = 50,
#'                          warranty.time = 36)
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "density")
#' 
#' PlotCumulativePredictTable(DeviceN.Enforced.weib,
#'                            plot.what = "cumulative")
#' 
#' }
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
