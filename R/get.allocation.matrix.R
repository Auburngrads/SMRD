#' Title
#'
#' @param accvar.list 
#' @param time.units 
#' @param times 
#' @param reps 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' InsulationBrkdwn.ADDTplan <- get.allocation.matrix(list(DegreesC = c(180,225,250,275)),
#'                                                    times = c(1,2,4,8,16,32,48,64),
#'                                                    time.units = "Weeks",
#'                                                    reps = 4)
#' 
#' plot(InsulationBrkdwn.ADDTplan)
#' 
#' InsulationBrkdwn.ADDTpv <- get.ADDT.plan.values(distribution = "normal",
#'                                                 transformation.x = "Arrhenius", 
#'                                                 transformation.Response = "log", 
#'                                                 transformation.time = "linear",
#'                                                 beta0 = 2.58850162033243,
#'                                                 beta1 = -476873415881.376,
#'                                                 beta2 = 1.41806367703643,
#'                                                 sigma = 0.172609,
#'                                                 time.units = "Weeks",
#'                                                 response.units = "Volts", 
#'                                                 FailLevel = 10, 
#'                                                 use.condition = 100)
#' 
#' print(InsulationBrkdwn.ADDTpv)
#' 
#' InsulationBrkdwn.vADDTplan <- hframe.to.vframe(InsulationBrkdwn.ADDTplan)
#' sum(allocation(InsulationBrkdwn.vADDTplan))
#' 
#' names(InsulationBrkdwn.ADDTpv)
#' 
#' InsulationBrkdwn.plan.sim.out <- sim.ADDT.test.plan(ADDT.test.plan = InsulationBrkdwn.ADDTplan, 
#'                                                     ADDT.plan.values = InsulationBrkdwn.ADDTpv, 
#'                                                     number.sim = 5)
#' 
#' ADDT.plot.time.v.x(InsulationBrkdwn.plan.sim.out)
#' 
#' ADDT.plot.Deg.v.Time(InsulationBrkdwn.plan.sim.out)
#' ADDT.plot.FracFail.v.Time(InsulationBrkdwn.plan.sim.out)
#' 
#' ADDT.vcv(ADDT.plan.values = InsulationBrkdwn.ADDTpv,
#'          ADDT.test.plan = hframe.to.vframe(InsulationBrkdwn.ADDTplan))
#' 
#' 
#' }
get.allocation.matrix <-
function (accvar.list, time.units, times, reps = 1) 
{
    frame <- expand.grid(accvar.list)
    levels.columns <- names(frame)
    time.names <- paste(time.units, times, sep = "")
    the.allocations <- matrix(as.numeric(reps), ncol = length(time.names), 
        nrow = nrow(frame))
    dimnames(the.allocations) <- list(NULL, time.names)
    frame <- data.frame(frame, the.allocations)
    attr(frame, "time.columns") <- time.names
    attr(frame, "time.units") <- time.units
    attr(frame, "levels.columns") <- levels.columns
    attr(frame, "frame.type") <- "hframe"
    oldClass(frame) <- c("ADDT.test.plan", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
