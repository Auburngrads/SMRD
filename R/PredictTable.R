#' Title
#'
#' @param mlest.out 
#' @param FtimeStart 
#' @param FtimeEnd 
#' @param warranty.time 
#' @param print.total 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' DeviceN.ld <- frame.to.ld(devicen,
#'                           response.column = "months", 
#'                           censor.column = "event",
#'                           case.weight.column = "counts")
#'                           
#' PredictTable(DeviceN.mlest.lnorm,
#'              FtimeStart = 0,
#'              FtimeEnd = 1)
#' 
#' PredictTable(DeviceN.mlest.lnorm,
#'              FtimeStart = 1,
#'              FtimeEnd = 2)
#' 
#' PredictTable(DeviceN.mlest.lnorm,
#'              FtimeStart = 0,
#'              FtimeEnd = 36)
#' 
#' PredictTable(DeviceN.mlest.lnorm,
#'              FtimeStart = 0,
#'              FtimeEnd = 36,
#'              warranty.time = 36)
#' 
#' DeviceN.mlest.weib <- mlest(DeviceN.ld,
#'                             dist = "Weibull")
#' 
#' PredictTable(DeviceN.mlest.weib,
#'              FtimeStart = 1,
#'              FtimeEnd = 2)
#' 
#' PredictTable(DeviceN.mlest.weib,
#'              FtimeStart = 0,
#'              FtimeEnd = 36)
#' 
#' }
PredictTable <-
function (mlest.out, FtimeStart, FtimeEnd, warranty.time = Inf, 
    print.total = TRUE) 
{
    data.ld <- mlest.out$data.ld
    atRiskTable <- data.ld[censor.codes(data.ld) == 2, , drop = FALSE]
    age <-Response(atRiskTable)
    NumberAtRisk <- case.weights(atRiskTable)
    time.lower <- pmin(age + FtimeStart, warranty.time)
    time.upper <- pmin(age + FtimeEnd, warranty.time)
    rho <- CondProbInterval2(mlest.out, age = age, tL = time.lower, 
        tU = time.upper)
    Efail <- rho * NumberAtRisk
    results <- data.frame(age, NumberAtRisk, rho, Efail)
    names(results) <- c("Age", "AtRisk", "rhohat", "Efail")
    attr(results, "Total") <- sum(Efail)
    if (print.total) 
        cat("Total=", attr(results, "Total"), "\n")
    results
}
