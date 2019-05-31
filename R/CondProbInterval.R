#' Title
#'
#' @param mu 
#' @param sigma 
#' @param distribution 
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
#' BearingCage.ld <- frame.to.ld(bearingcage,
#'                               response.column = 1, 
#'                               censor.column = 2,
#'                               case.weight.column = 3)
#' 
#' BearingCage.mlest.weib <- mlest(BearingCage.ld,
#'                                 dist = "Weibull")
#' 
#' CondProbInterval2(BearingCage.mlest.weib,
#'                   age = 50,
#'                   tL = 50,
#'                   tU = 350)
#' 
#' CondProbInterval2(BearingCage.mlest.weib,
#'                   age = 150,
#'                   tL = 150,
#'                   tU = 450)
#' 
#' # Table 12.1 from Meeker and Escobar (1998)
#' 
#' PredictTable(BearingCage.mlest.weib,
#'              FtimeStart = 0, 
#'              FtimeEnd = 300)
#' 
#' CondProbInterval(mu = 3.7393, 
#'                  sigma = 0.7639,
#'                  distribution = "lognormal",
#'                  age = 1,
#'                  tL = 1,
#'                  tU = 36)
#' 
#' }
CondProbInterval <-
function (mu, sigma, distribution, age, tL, tU) 
{
    age.gt.tL <- age > tL
    if (any(age.gt.tL)) 
        stop(paste("age=", age[age.gt.tL], ">", "tL=", tL[age.gt.tL]))
    tL.gt.tU <- tL > tU
    if (any(tL.gt.tU)) 
        stop(paste("tL=", tL[tL.gt.tU], ">", "tU=", tU[tL.gt.tU]))
    zC <- (log(age) - mu)/sigma
    zL <- (log(tL) - mu)/sigma
    zU <- (log(tU) - mu)/sigma
    (wqmf.phibf(zU, distribution) - wqmf.phibf(zL, distribution))/wqmf.phibm(zC, 
        distribution)
}
