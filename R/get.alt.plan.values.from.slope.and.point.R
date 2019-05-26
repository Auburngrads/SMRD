#' Title
#'
#' @param distribution 
#' @param relationship 
#' @param slope 
#' @param probs 
#' @param accelvar 
#' @param censor.time 
#' @param beta 
#' @param sigma 
#' @param time.units 
#' @param accelvar.units 
#' @param power 
#' @param use.conditions 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AdhesiveBond.Weibull.altpv <- get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
#'                                                                        relationship = "Arrhenius", 
#'                                                                        accelvar.units = c("DegreesC"),
#'                                                                        time.units = "Days", 
#'                                                                        censor.time = 183,
#'                                                                        probs = c(.001), 
#'                                                                        accelvar = c(50),
#'                                                                        slope = 0.726, 
#'                                                                        beta = 1.667)
#' 
#' print(AdhesiveBond.Weibull.altpv)
#' 
#' AdhesiveBond1.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(78,98,120),
#'                                                   number.of.units = c(155,60,84),
#'                                                   censor.times = c(183,183,183))
#' 
#' AdhesiveBond2.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
#'                                                   number.of.units = c(212,88,0),
#'                                                   censor.times = c(243,243,243))
#' 
#' AdhesiveBond3.altplan <- get.alt.test.plan.direct(accel.variable.levels = c(80,100,120),
#'                                                   number.of.units = c(150,60,90),
#'                                                   censor.times = c(243,243,243))
#' 
#' plot(AdhesiveBond1.altplan,
#'      ALT.plan.values = AdhesiveBond.Weibull.altpv,
#'      use.condition = 50)
#' 
#' plot(AdhesiveBond2.altplan,
#'      ALT.plan.values = AdhesiveBond.Weibull.altpv,
#'      use.condition = 50)
#' 
#' plot(AdhesiveBond3.altplan,
#'      ALT.plan.values = AdhesiveBond.Weibull.altpv,
#'      use.condition = 50)
#' 
#' }
get.alt.plan.values.from.slope.and.point <-
function (distribution, relationship, slope, probs, accelvar, 
    censor.time, beta, sigma, time.units = "Time", accelvar.units, 
    power = NULL, use.conditions = NULL) 
{
    relationship <- set.relationship.power(relationship, power)
    if (missing(accelvar.units)) {
        accelvar.units <- rep("", length(relationship))
        for (i in 1:length(accelvar.units)) switch(generic.relationship.name(relationship[i]), 
            Arrhenius = {
                accelvar.units[i] <- "Degrees C"
            }, humidity = {
                accelvar.units[i] <- "RH"
            }, stop(paste("Need to specify units for accelerating variable for relationship", 
                relationship[i])))
    }
    if (missing(sigma)) {
        if (missing(beta)) {
            cat("Both sigma and beta missing. Assuming that beta=1 \n \n")
            sigma <- 1
        }
        sigma <- 1/beta
  } else {
        if (!(missing(beta))) {
            stop("Cannot specify both sigma and beta")
        }
    }
    if (length(probs) > 1) {
        stop("length of probs should be only 1")
    }
    if (length(accelvar) != length(relationship)) {
        stop(paste("length of accelvar is", length(accelvar), 
            " but it should be", length(relationship)))
    }
    mu1 <- logb(censor.time) - quant(probs[1], distribution) * 
        sigma
    x <- rep(NA, length(relationship))
    for (i in 1:length(relationship)) {
        x[i] <- f.relationship(accelvar[i], subscript.relationship(relationship, 
            i))
    }
    betavec <- slope
    beta0 <- mu1 - sum(betavec * x)
    beta <- 1/sigma
    theta.vec <- c(beta0 = beta0, beta = betavec, sigma = sigma)
    rlist <- list(distribution = distribution, relationship = relationship, 
        beta0 = beta0, betavec = betavec, beta = beta, sigma = sigma, 
        probs = probs, accelvar = accelvar, censor.time = censor.time, 
        accelvar.units = accelvar.units, theta.vec = theta.vec, 
        time.units = time.units, use.conditions = use.conditions)
    oldClass(rlist) <- "alt.plan.values"
    return(rlist)
}
