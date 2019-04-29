extend.menu.FACTOR.groupm.mleprobplot <-
function (data.ld, distribution, x.min, x.max, xlab, y.min, y.max,
    ylab, conf.level, grids, linear.axes, slope.axis, title.option,
    trunc.correct, plot.censored.ticks, band.type, make.quant.table,
    make.cdf.table, print.digits, time.L, time.U, quantile.list,
    HowcdfTimesList, cdfTimesList, stress.var, table.stress.var.list,
    DoStressPlotResponseOnXaxis, censor.time = NULL, quant.lines,
    stress.x.min, stress.x.max, stress.xlab, stress.y.min, stress.y.max,
    stress.ylab, plot.ci.on.list, stress.grids, do.legend, include.data,
    theta.start = NULL, parameter.fixed = NULL)
{
    if (!any(CheckClass("life.data", data.ld))) {
        func.call$data.ld <- data.ld[[1]]
        warning(paste(oldClass(data.ld), "check possible problem \n"))

    }
    func.call <- match.call()
    group.var <- match(ClistToVec(stress.var), dimnames(xmat(data.ld))[[2]])
    func.call[[1]] <- as.name("groupm.mleprobplot")
    if (band.type == "None" || band.type == "none") {
        band.type <- "n"
        ci.list <- NULL
}   else {
        all.stresses <- get.x.markers(data.ld, group.var = group.var)
        ci.list <- match(ClistToVec(plot.ci.on.list), all.stresses)
    }
    all.stresses <- get.x.markers(data.ld, group.var = group.var)
    all.stresses.long <- get.x.markers(data.ld, group.var = group.var,
        long = T)
    xnames.vector <- dimnames(xmat(data.ld)[, group.var, drop = F])[[2]]
    new.xnames <- paste(xnames.vector, collapse = "")
    the.xmat <- vector.strip.blanks(apply(xmat(data.ld)[, group.var,
        drop = F], 1, paste, xnames.vector, collapse = ";", sep = ""))
    the.xmat <- as.matrix(the.xmat)
    if (length(unique(the.xmat)) <= 1) {
        stop("The number of categories with valid censored data\n must be two or more in order to fit the requested model")
    }
    dimnames(the.xmat)[[2]] <- new.xnames
    xlabel(data.ld) <- new.xnames
    xmat(data.ld) <- the.xmat
    func.call$data.ld <- data.ld
    func.call$group.var <- 1
    func.call$relationship <- "class"
    func.call$response.on.yaxis <- ResponseOnXaxis
    if (table.stress.var.list == "None") {
        table.stress.var.list <- NULL
        table.stress.var.frame <- NULL
}   else {
        table.stress.var.vec <- ClistToVec(table.stress.var.list)
        the.match <- match(table.stress.var.vec, all.stresses)
        nonestimable.indices <- (1:length(the.match))[is.na(the.match)]
        if (!is.null(nonestimable.indices))
            nonestimable.list <- table.stress.var.vec[nonestimable.indices]
        else nonestimable.list <- NULL
        the.x.names <- colnames(xmat(data.ld))[group.var]
        the.match.levels <- paste(all.stresses.long[match(ClistToVec(table.stress.var.list),
            all.stresses)], collapse = ",")
        table.stress.var.frame <- string.to.frame(the.match.levels,
            col.names = the.x.names)
    }
    old.digits <- options(digits = print.digits)
    on.exit(options(old.digits))
    cdfTimesList <- FixHowcdfTimesList(HowcdfTimesList, cdfTimesList)
    func.call <- StringToArg(func.call, cdfTimesList, "Automatic",
        numeric = T)
    func.call <- StringToArg(func.call, quantile.list, "Automatic",
        numeric = T)
    time.range <- c(time.L, time.U)
    func.call$time.range <- time.range
    conf.level <- conf.level/100
    func.call$conf.level <- conf.level
    func.call$ci.list <- ci.list
    func.call$band.type <- band.type
    func.call <- StringToArg(func.call, plot.censored.ticks,
        "no")
    func.call <- StringToArg(func.call, xlab, "Automatic")
    func.call <- StringToArg(func.call, ylab, "Automatic")
    func.call <- FixXrangeYrange(func.call)
    invisible()
    if (distribution == "6 distributions") {
        func.call[[1]] <- as.name("six.groupm.mleprobplot")
        mlest.out.args <- !is.na(match(names(func.call), names(args(six.groupm.mleprobplot))))
        the.func.call <- func.call[mlest.out.args]
        groupm.mlest.out <- eval(the.func.call, envir = .frame0)
}   else {
        func.call[[1]] <- as.name("groupm.mleprobplot")
        mlest.out.args <- !is.na(match(names(func.call), names(args(groupm.mleprobplot))))
        the.func.call <- func.call[mlest.out.args]
        groupm.mlest.out <- eval(the.func.call, envir = .frame0)
        summary(groupm.mlest.out, digits = print.digits, conf.level = conf.level)
        if (make.cdf.table || make.quant.table && !is.null(table.stress.var.frame)) {
            if (length(nonestimable.list) > 0)
                cat("\n***Nonestimable subsets:", paste(nonestimable.list,
                  collapse = ","), "\n")
            for (i in 1:nrow(table.stress.var.frame)) {
                table.new.data <- as.data.frame(paste(as.matrix(table.stress.var.frame[i,
                  , drop = F]), collapse = ";"))
                names(table.new.data) <- colnames(xmat(groupm.mlest.out[[1]]$data.ld))
                if (make.cdf.table) {
                  tmp <- failure.probabilities.groupm.out(groupm.mlest.out,
                    digits = print.digits, time.vec = func.call$cdfTimesList,
                    new.data = table.new.data, conf.level = conf.level)
                  print(tmp)
                }
                if (make.quant.table) {
                  tmp <- quantiles.groupm.out(groupm.mlest.out,
                    digits = print.digits, prob.vec = func.call$quantile.list,
                    new.data = table.new.data, conf.level = conf.level)
                  print(tmp)
                }
            }
        }
        if (DoStressPlot) {
            func.call <- StringToArg(func.call, stress.xlab,
                "Automatic")
            func.call <- StringToArg(func.call, stress.ylab,
                "Automatic")
            xlim <- c(stress.x.min, stress.x.max)
            ylim <- c(stress.y.min, stress.y.max)
            quant.lines <- as.numeric(ClistToVec(quant.lines))
            plot.alt.fit(groupm.mlest.out, censor.time = censor.time,
                quant.lines = quant.lines, xlim = xlim,
                ylim = ylim, xlab = func.call$stress.xlab,
                ylab = func.call$stress.ylab, title.option = GetSMRDDefault("SMRD.TitleOption"),
                grids = func.call$stress.grids, response.on.yaxis = ResponseOnXaxis,
                include.data = include.data)
        }
        return(groupm.mlest.out)
    }
}
