#' Title
#'
#' @param mlest.out 
#' @param age 
#' @param tL 
#' @param tU 
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
#' DeviceN.mlest.lnorm <- mlest(DeviceN.ld,
#'                              dist = "Lognormal")
#' 
#' CondProbInterval2(DeviceN.mlest.lnorm,
#'                   age = 1,
#'                   tL = 1,
#'                   tU = 36)
#' CondProbInterval2(DeviceN.mlest.lnorm,
#'                   age = 2,
#'                   tL = 2,
#'                   tU = 36)
#' CondProbInterval2(DeviceN.mlest.lnorm,
#'                   age = 1,
#'                   tL = 1,
#'                   tU = 2)
#' CondProbInterval2(DeviceN.mlest.lnorm,
#'                   age = 2,
#'                   tL = 2,
#'                   tU = 3)
#' }
CondProbInterval2 <-
function (mlest.out, age, tL, tU) 
{
    theta.hat <- mlest.out$theta.hat
    result <- CondProbInterval(mu = theta.hat[1], sigma = theta.hat[2], 
        mlest.out$distribution, age, tL, tU)
    attributes(result) <- NULL
    result
}
