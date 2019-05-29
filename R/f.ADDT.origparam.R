#' Title
#'
#' @param gamma 
#' @param model 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AdhesiveBondC.ADDTpv <-
#'   get.ADDT.plan.values(distribution = "sev",
#'                        transformation.x = c("Arrhenius","Humidity"),
#'                        transformation.response = "log",
#'                        transformation.time = "Square root",
#'                        beta0 = 2.168,
#'                        the.slope = -0.00709030595,
#'                        slope.at = c(30,50),
#'                        beta2 = c(0.6666,.2),
#'                        sigma = 0.1807,
#'                        time.units = "Weeks",
#'                        response.units = "Pounds",
#'                        FailLevel = 2,
#'                        use.condition = "30;50")
#' 
#' AdhesiveBondC.ADDTplan <-
#'   get.allocation.matrix(list(DegreesC = c(40,50,60),RH = c(20,80)),
#'                         times = c(1,2,5,10,20,50),
#'                         time.units = "Weeks",
#'                         reps = 6)
#' 
#' tmp.pmodel <- 
#'   pseudo.model((ADDT.plan.values = AdhesiveBondC.ADDTpv,
#'                ADDT.test.plan = hframe.to.vframe(AdhesiveBondC.ADDTplan))
#' 
#' f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, 
#'                    tmp.pmodel)
#' 
#' f.ADDT.origparam(f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, tmp.pmodel), tmp.pmodel)
#' 
#' }
f.ADDT.origparam <-
function (gamma, model) 
{
    gamma2.indices <- seq(3, length(model$xbar) + 2)
    gamma2.vec <- gamma[gamma2.indices]
    beta2.vec <- gamma2.vec
    beta.x <- sum(beta2.vec * model$xbar)
    beta0 <- gamma[1] - gamma[2] * model$tbar
    beta1 <- gamma[2] * exp(-beta.x)
    sigma <- exp(gamma[length(gamma)])
    thetaorig <- c(beta0, beta1, beta2.vec, sigma)
    names(thetaorig) <- model$orig.param.names
    return(thetaorig)
}
