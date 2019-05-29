#' Title
#'
#' @param ADDT.plan.values 
#' @param ADDT.test.plan 
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
#'   pseudo.model(ADDT.plan.values = AdhesiveBondC.ADDTpv,
#'                ADDT.test.plan = hframe.to.vframe(AdhesiveBondC.ADDTplan))
#' 
#' f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, 
#'                    tmp.pmodel)
#' 
#' f.ADDT.origparam(f.ADDT.stableparam(AdhesiveBondC.ADDTpv$theta.vec, tmp.pmodel), tmp.pmodel)
#' 
#' }
pseudo.model <-
function (ADDT.plan.values, ADDT.test.plan)
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

    transformation.time <- ADDT.plan.values$transformation.time
    transformation.x <- fix.inverse.relationship(ADDT.plan.values$transformation.x)
    levels <- AT.levels(ADDT.test.plan)
    number.accelerators <- ncol(levels)
    xbar <- rep(NA, number.accelerators)
    x.tran <- levels
    for (i in 1:number.accelerators) {
        x.tran[, i] <- multiple.f.relationship(levels[, i], subscript.relationship(transformation.x,
            i))
        the.allocations <- allocation(ADDT.test.plan)
        xbar[i] <- wmean(x.tran[, i], the.allocations)
    }
    beta2.names <- paste("beta", seq(2, length(ADDT.plan.values$accelvar.units) +
        1), sep = "")
    gamma2.names <- paste("gamma", seq(2, length(ADDT.plan.values$accelvar.units) +
        1), sep = "")
    orig.param.names <- c("beta0", "beta1", beta2.names, "sigma")
    stable.param.names <- c("gamma0", "gamma1", gamma2.names,
        "logsigma")
    list(tbar = wmean(f.relationship(times(ADDT.test.plan), transformation.time),
        allocation(ADDT.test.plan)), xbar = xbar, stable.param.names = stable.param.names,
        orig.param.names = orig.param.names)
}
