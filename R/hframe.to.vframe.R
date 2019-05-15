#' Title
#'
#' @param hframe 
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
hframe.to.vframe <-
function (hframe) 
{
    accel.var.columns <- attr(hframe, "levels.columns")
    time.columns <- attr(hframe, "time.columns")
    x.names <- names(hframe[, accel.var.columns, drop = F])
    the.x.combinations <- as.matrix(hframe[, accel.var.columns, 
        drop = F])
    the.allocation.matrix <- hframe[, time.columns, drop = F]
    the.time.names <- names(the.allocation.matrix)
    the.allocation.matrix <- as.matrix(the.allocation.matrix)
    the.times <- tparse(the.time.names)
    time.units <- attr(the.times, "Time.units")
    the.vframe <- matrix(NA, ncol = ncol(the.x.combinations) + 
        2, nrow = ncol(the.allocation.matrix) * nrow(the.allocation.matrix))
    krow <- 0
    alloc.col <- ncol(the.vframe)
    for (i in 1:nrow(the.x.combinations)) {
        for (j in 1:length(the.times)) {
            if (the.allocation.matrix[i, j] >= 0) {
                krow <- krow + 1
                the.vframe[krow, ncol(the.x.combinations) + 1] <- the.times[j]
                the.vframe[krow, alloc.col] <- as.numeric(as.character(the.allocation.matrix[i, 
                  j]))
                the.vframe[krow, 1:(ncol(the.x.combinations))] <- as.vector(the.x.combinations[i, 
                  ])
            }
        }
    }
    dimnames(the.vframe) <- list(NULL, c(x.names, time.units, 
        "Allocation"))
    the.vframe <- as.data.frame(the.vframe)
    for (j in 1:ncol(the.vframe)) {
        try.numeric <- as.numeric.nocheck(the.vframe[, j])
        if (!any(is.na(try.numeric))) 
            the.vframe[, j] <- try.numeric
    }
    oldClass(the.vframe) <- c("ADDT.vframe", "data.frame")
    MysetOldClass(attr(the.vframe, "class"))
    attr(the.vframe, "frame.type") <- "vframe"
    attr(the.vframe, "allocation.column") <- "Allocation"
    attr(the.vframe, "time.columns") <- time.units
    attr(the.vframe, "time.units") <- time.units
    attr(the.vframe, "levels.columns") <- dimnames(the.x.combinations)[[2]]
    return(the.vframe)
}
