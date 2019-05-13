#' Title
#'
#' @param accel.variable.levels 
#' @param number.of.units 
#' @param censor.times 
#' @param accelvar.names 
#' @param describe.string 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' advbond.model1 <- 
#'   get.alt.plan.values.from.two.points(
#'     distribution = "Weibull",
#'     relationship = "Inverse Power Rule", 
#'     time.units = "hours", 
#'     censor.time = 1000,
#'     probs = c(.001,.9), 
#'     accelvar.units = "volts", 
#'     accelvar = c(110,150), 
#'     beta = 1.667)
#' 
#' advbond.test.plan1 <- 
#'   get.alt.test.plan.direct(accel.variable.levels = c(130,140,150),
#'                            number.of.units = c(100,100,100))
#' 
#' plot(advbond.test.plan1,
#'      ALT.plan.values = advbond.model1, 
#'      my.title = "",
#'      use.conditions = 120)
#' 
#' advbond.model2 <- 
#'   get.alt.plan.values.from.two.points(distribution = "Lognormal",	
#'                                       relationship = "Arrhenius", 
#'                                       time.units = "days",
#'                                       censor.time = 183,
#'                                       probs = c(.001,.9),
#'                                       accelvar = c(50,120),
#'                                       sigma = .5)
#' 
#' 
#' advbond.test.plan2 <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
#'                          number.of.units = c(100,100,100))
#' 
#' plot(advbond.test.plan2, 
#'      ALT.plan.values = advbond.model2, 
#'      my.title = "",
#'      use.condition = 50)
#' }

get.alt.test.plan.direct <-
function (accel.variable.levels, number.of.units, censor.times = NULL, 
    accelvar.names = "accel.variable", describe.string = "") 
{
    accel.variable.levels <- as.matrix(accel.variable.levels)
    if (length(accelvar.names) != ncol(accel.variable.levels)) {
        accelvar.names <- paste(accelvar.names, 1:ncol(accel.variable.levels), 
            sep = "")
    }
    the.dim.names <- list(rep("", nrow(accel.variable.levels)), 
        accelvar.names)
    dimnames(accel.variable.levels) <- the.dim.names
    if (is.null(censor.times)) 
        censor.times <- rep(NA, nrow(accel.variable.levels))
    rlist <- data.frame(accel.variable.levels, number.units = number.of.units, 
        censor.times = censor.times)
    if (any(number.of.units == 0)) {
        the.positives <- number.of.units > 0
        rlist <- rlist[the.positives, ]
    }
    attr(rlist, "accelvar.names") <- accelvar.names
    attr(rlist, "levels.columns") <- vector.strip.blanks(accelvar.names, 
        FillChar = ".")
    attr(rlist, "allocation.column") <- "number.units"
    attr(rlist, "describe.string") <- describe.string
    oldClass(rlist) <- c("alt.test.plan", "data.frame")
    MysetOldClass(attr(rlist, "class"))
    return(rlist)
}
