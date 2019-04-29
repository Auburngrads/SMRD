extend.menu.ALTmufloat.groupm.mleprobplot <-
function (data.ld, distribution, x.min, x.max, xlab, y.min, y.max,
    ylab, conf.level, grids, linear.axes, slope.axis, title.option,
    trunc.correct, plot.censored.ticks, band.type, make.quant.table,
    make.cdf.table, print.digits, time.L, time.U, quantile.list,
    HowcdfTimesList, cdfTimesList, stress.var, table.stress.var.list,
    DoStressPlot,ResponseOnXaxis, censor.time = NULL, quant.lines,
    stress.x.min, stress.x.max, stress.xlab, stress.y.min, stress.y.max,
    stress.ylab, plot.ci.on.list, stress.grids, do.legend, include.data,
    theta.start = NULL, parameter.fixed = NULL)
{
  `not.stripped<-` <-
    function (data.ld, value)
    {
      attr(data.ld, "not.stripped") <- value
      return(data.ld)
    }

    func.call <- match.call()
    dump <- T
    group.var <- match(ClistToVec(stress.var), dimnames(xmat(data.ld))[[2]])
    ordered.stresses <- get.x.markers(data.ld, group.var = group.var,
        do.order = T)
    unordered.stresses <- get.x.markers(data.ld, group.var = group.var,
        do.order = F)
    stress.order <- match(ordered.stresses, unordered.stresses)
    ordered.stresses <- unordered.stresses[stress.order]
    data.ld <- strip.bad.subsets(data.ld, group.var = group.var)
    the.xmat <- as.data.frame(xmat(data.ld))
    new.ordered.stresses <- get.x.markers(data.ld, group.var = group.var,
        do.order = T)
    new.unordered.stresses <- get.x.markers(data.ld, group.var = group.var,
        do.order = F)
    not.stripped <- !is.na(match(ordered.stresses, new.ordered.stresses))
    not.stripped(data.ld) <- not.stripped
    stress.order(data.ld) <- match(new.ordered.stresses, new.unordered.stresses)
    func.call$data.ld <- data.ld
    func.call$relationship <- rep("class", length(stress.var))
    group.var <- match(stress.var, dimnames(xmat(data.ld))[[2]])
    func.call$group.var <- group.var
    func.call[[1]] <- as.name("extend.menu.FACTOR.groupm.mleprobplot")
    mlest.out.args <- !is.na(match(names(func.call), names(args(extend.menu.FACTOR.groupm.mleprobplot))))
    the.func.call <- func.call[mlest.out.args]
    groupm.mlest.out <- eval(the.func.call, envir = .frame0)
    return(groupm.mlest.out)
}
