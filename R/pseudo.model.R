.pseudo.model <-
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
