#' Title
#'
#' @param beta 
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
f.ADDT.stableparam <-
function (beta, model) 
{
    beta2.names <- paste("beta", seq(2, length(model$xbar) + 
        1), sep = "")
    beta2.vec <- beta[beta2.names]
    if (F) {
        cat("beta2.vec=", beta2.vec, "\n")
        cat("beta2.names=", beta2.names, "\n")
        cat("beta=", beta, "\n")
        cat("names(beta)=", names(beta), "\n")
        cat("beta2.vec=", beta2.vec, "\n")
        cat("model$xbar=", model$xbar, "\n")
    }
    beta.x <- sum(beta2.vec * model$xbar)
    gamma0 <- beta[1] + beta[2] * model$tbar * exp(beta.x)
    gamma2.vec <- beta2.vec
    gamma1 <- beta[2] * exp(beta.x)
    sigma <- logb(beta[length(beta)])
    thetastable <- c(gamma0, gamma1, gamma2.vec, sigma)
    names(thetastable) <- model$stable.param.names
    return(thetastable)
}
