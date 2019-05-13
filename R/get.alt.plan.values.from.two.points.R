#' Title
#'
#' @param distribution 
#' @param relationship 
#' @param probs 
#' @param accelvar 
#' @param censor.time 
#' @param beta 
#' @param sigma 
#' @param time.units 
#' @param accelvar.units 
#' @param power 
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
get.alt.plan.values.from.two.points <-
function (distribution, relationship, probs, accelvar, censor.time,
    beta, sigma, time.units = "Time", accelvar.units, power = NULL)
{
    relationship <- set.relationship.power(relationship, power)
    if (missing(accelvar.units)) {
        switch(relationship, Arrhenius = , arrhenius = {
            accelvar.units <- "Degrees C"
        }, humidity = {
            accelvar.units <- "RH"
        }, stop("Need to specify units for accelerating variable"))
    }
    if (missing(sigma)) {
        if (missing(beta)) {
            cat("Both sigma and beta missing. Assuming that beta=1 \n \n")
            sigma <- 1
        }
        sigma <- 1/beta
}   else {
        if (!(missing(beta))) {
            stop("Cannot specify both sigma and beta")
        }
    }
    mu1 <- logb(censor.time) - quant(probs[1], distribution) *
        sigma
    mu2 <- logb(censor.time) - quant(probs[2], distribution) *
        sigma
    x1 <- f.relationship(accelvar[1], relationship)
    x2 <- f.relationship(accelvar[2], relationship)
    beta1 <- (mu2 - mu1)/(x2 - x1)
    beta0 <- mu2 - beta1 * x2
    beta <- 1/sigma
    theta.vec <- c(beta0 = beta0, beta1 = beta1, sigma = sigma)
    rlist <- list(distribution = distribution, relationship = relationship,
        beta0 = beta0, beta1 = beta1, beta = beta, sigma = sigma,
        probs = probs, accelvar = accelvar, censor.time = censor.time,
        accelvar.units = accelvar.units, theta.vec = theta.vec,
        time.units = time.units)
    oldClass(rlist) <- "alt.plan.values"
    return(rlist)
}
