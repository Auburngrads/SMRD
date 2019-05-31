#' Title
#'
#' @param ALT.test.plan 
#' @param ALT.plan.values 
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
#' plot(AdhesiveBond1.altplan,
#'      ALT.plan.values = AdhesiveBond.Weibull.altpv,
#'      use.condition = 50)
#' 
#' ALT.vcv(AdhesiveBond1.altplan,
#'         ALT.plan.values = AdhesiveBond.Weibull.altpv)
#' 
#' evaluate(AdhesiveBond1.altplan,
#'          ALT.plan.values = AdhesiveBond.Weibull.altpv,
#'          quantile.of.interest = 0.5,
#'          use.condition = 50)
#' 
#' 
#' }
ALT.vcv <-
function (ALT.test.plan, ALT.plan.values)
{
  AT.levels <-
    function (ADDT.test.plan)
    {
      levels.columns <- attr(ADDT.test.plan, "levels.columns")
      levels <- ADDT.test.plan[, levels.columns, drop = F]
      col.names <- dimnames(ADDT.test.plan)[[2]]
      names(col.names) <- col.names
      dimnames(levels) <- list(as.character(1:nrow(levels)), col.names[levels.columns])
      oldClass(levels) <- "data.frame"
      return(levels)
    }

    levels <- AT.levels(ALT.test.plan)
    number.accelerators <- ncol(levels)
    theta.vec <- ALT.plan.values$theta.vec
    number.parameters <- 2 + number.accelerators
    if (number.parameters != length(theta.vec))
        stop("2+number.accelerators !=length(theta.vec)")
    beta0 <- theta.vec[1]
    beta1.names <- paste("beta", seq(2, length(ALT.plan.values$accelvar.units) +
        1), sep = "")
    beta1.vec <- theta.vec[beta1.names]
    sigma <- theta.vec[length(theta.vec)]
    distribution <- ALT.plan.values$distribution
    relationship <- ALT.plan.values$relationship
    
    if (length(relationship) != number.accelerators) {
      
        stop("\nlength(relationship) {",
                   length(relationship),
                   "} != number.accelerators {",
            number.accelerators,"}")
      
    }
    
    the.allocations <- allocation(ALT.test.plan)[, 1]
    x.tran <- levels
    for (i in 1:number.accelerators) {
        x.tran[, i] <- multiple.f.relationship(levels[, i], subscript.relationship(relationship,
            i))
    }
    fisher <- matrix(0, number.parameters, number.parameters)
    param.names <- names(theta.vec)
    gamma1.names <- paste("gamma", seq(2, number.accelerators +
        1), sep = "")
    sigma <- ALT.plan.values$theta.vec[length(ALT.plan.values$theta.vec)]
    if (is.logdist(distribution))
        censor.time <- logb(ALT.test.plan$censor.times)
    else censor.time <- ALT.test.plan$censor.times
    std.cen <- rep(NA, length = length(censor.time))
    fail.prob <- rep(NA, length = length(censor.time))
    for (i in 1:nrow(ALT.test.plan)) {
        x.vector <- as.vector(as.matrix(x.tran)[i, ])
        std.cen[i] <- (censor.time[i] - sum(c(1, x.vector) *
            theta.vec[-length(theta.vec)]))/sigma
        fail.prob[i] <- wqmf.phibf(std.cen[i], distribution)
        lsinf.out <- lsinf(std.cen[i], "right", ALT.plan.values$distribution)
        fishersub <- matrix(c(lsinf.out$f11, lsinf.out$f12, lsinf.out$f12,
            lsinf.out$f22), 2, 2)
        dD.dgamma0 <- 1
        dD.dgamma1 <- x.vector
        grad <- c(dD.dgamma0, dD.dgamma1)
        M <- cbind(c(grad, 0), c(rep(0, length(grad)), 1))
        fisheri <- M %*% fishersub %*% t(M)
        fisher <- fisher + the.allocations[i] * fisheri
        if (F) {
            cat("\n********\nLevels=", levels[i, ], "\n")
            cat("tLevels=", format(x.vector), "\n")
            cat("stdcen=", format(std.cen[i]), "\n")
            cat("allocation=", format(the.allocations[i]), "\n")
            cat("failure prob=", format(fail.prob[i]), "\n")
        }
    }
    dimnames(fisher) <- list(param.names, param.names)
    the.vcv <- my.solve(fisher/sigma^2)
    plan.table <- data.frame(levels, 
                             cbind(n = the.allocations,
                                   ctime = ALT.test.plan$censor.times, 
                                   zeta = round(std.cen,2), 
                                   p = fail.prob, 
                                   efail = round(the.allocations *fail.prob, 2)))
    
    return(list(the.fim = fisher, 
                the.vcv = the.vcv, 
                plan.table = plan.table))
    
}
