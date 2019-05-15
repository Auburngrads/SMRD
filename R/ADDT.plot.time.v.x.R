#' Title
#'
#' @param results.object 
#' @param use.condition 
#' @param FailLevel 
#' @param plan.values.string 
#' @param plan.string 
#' @param quantile.list 
#' @param ylim 
#' @param xlim 
#' @param xlab 
#' @param ylab 
#' @param my.title 
#' @param title.option 
#' @param grids 
#' @param numplotsim 
#' @param nxpoints 
#' @param response.on.yaxis 
#' @param cex 
#' @param focus.variable 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' InsulationBrkdwn.ADDTplan <- get.allocation.matrix(list(DegreesC = c(180,225,250,275)),
#'                                                    times = c(1,2,4,8,16,32,48,64),
#'                                                    time.units = "Weeks",
#'                                                    reps = 4)
#' 
#' plot(InsulationBrkdwn.ADDTplan)
#' 
#' InsulationBrkdwn.ADDTpv <- get.ADDT.plan.values(distribution = "normal",
#'                                                 transformation.x = "Arrhenius", 
#'                                                 transformation.Response = "log", 
#'                                                 transformation.time = "linear",
#'                                                 beta0 = 2.58850162033243,
#'                                                 beta1 = -476873415881.376,
#'                                                 beta2 = 1.41806367703643,
#'                                                 sigma = 0.172609,
#'                                                 time.units = "Weeks",
#'                                                 response.units = "Volts", 
#'                                                 FailLevel = 10, 
#'                                                 use.condition = 100)
#' 
#' print(InsulationBrkdwn.ADDTpv)
#' 
#' InsulationBrkdwn.vADDTplan <- hframe.to.vframe(InsulationBrkdwn.ADDTplan)
#' sum(allocation(InsulationBrkdwn.vADDTplan))
#' 
#' names(InsulationBrkdwn.ADDTpv)
#' 
#' InsulationBrkdwn.plan.sim.out <- sim.ADDT.test.plan(ADDT.test.plan = InsulationBrkdwn.ADDTplan, 
#'                                                     ADDT.plan.values = InsulationBrkdwn.ADDTpv, 
#'                                                     number.sim = 5)
#' 
#' ADDT.plot.time.v.x(InsulationBrkdwn.plan.sim.out)
#' 
#' ADDT.plot.Deg.v.Time(InsulationBrkdwn.plan.sim.out)
#' ADDT.plot.FracFail.v.Time(InsulationBrkdwn.plan.sim.out)
#' 
#' ADDT.vcv(ADDT.plan.values = InsulationBrkdwn.ADDTpv,
#'          ADDT.test.plan = hframe.to.vframe(InsulationBrkdwn.ADDTplan))
#' 
#' 
#' }
ADDT.plot.time.v.x <-
function (results.object, use.condition = NULL, FailLevel = NULL,
    plan.values.string = NULL, plan.string = NULL, quantile.list = c(0.5),
    ylim = c(NA, NA), xlim = c(NA, NA), xlab = NULL, ylab = NULL,
    my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F, numplotsim = 50,
    nxpoints = 50, response.on.yaxis = T, cex = 1, focus.variable = 1)
{
    number.sim <- nrow(results.object)
    if (is.null(use.condition))
        use.condition <- attr(results.object, "use.condition")
    if (is.null(use.condition))
        stop("Use condition not specified")
    if (is.character(use.condition))
        use.condition <- string.to.frame(use.condition)
    character.use.condition <- as.character(use.condition)
    character.use.condition[focus.variable] <- ""
    if (is.null(FailLevel))
        FailLevel <- attr(results.object, "FailLevel")
    ADDT.plan.values <- attr(results.object, "plan.values")
    ADDT.test.plan <- attr(results.object, "plan")
    par(err = -1)
    if (is.null(plan.string))
        plan.string <- attr(results.object, "plan.string")
    if (is.null(plan.values.string))
        plan.values.string <- attr(results.object, "plan.values.string")
    FailLevelDef <- paste(FailLevel, get.response.units(ADDT.plan.values))
    if (is.null(xlab))
        xlab <- ADDT.plan.values$accelvar.units[focus.variable]
    if (is.null(ylab))
        ylab <- get.time.units(ADDT.plan.values)
    distribution <- ADDT.plan.values$distribution
    transformation.x <- ADDT.plan.values$transformation.x
    transformation.time <- ADDT.plan.values$transformation.time
    transformation.response <- ADDT.plan.values$transformation.response
    model.string <- paste("Resp:", transformation.response, ",Time:",
        transformation.time, ",x:", paste(ADDT.plan.values$transformation.x,
            character.use.condition, collapse = ","), ", Dist:",
        distribution, sep = "")
    if (map.SMRDDebugLevel() >= 5) {
        cat("\nbefore my.title def\n")
    }
    if (is.null(my.title))
        my.title <- paste("Accelerated destructive degradation test simulation based on\n",
            plan.string, plan.values.string, "\nFailure time",
            quantile.list[1], "quantile vs", xlab, "for failure definition",
            FailLevelDef, "\n", model.string)
    if (map.SMRDDebugLevel() >= 5) {
        cat("\nafter my.title def\n")
        browser()
    }
    transformation.x <- fix.inverse.relationship(transformation.x)
    slope.name <- attr(transformation.x, "slope.name")
    y.axis <- "log"
    the.levels.columns <- attr(ADDT.test.plan, "levels.columns")
    the.x <- ADDT.test.plan[, the.levels.columns, drop = F]
    if (map.SMRDDebugLevel() >= 4) {
        cat("use.condition in plot.time.v.x use.condition:")
        print(use.condition)
    }
    x.derived.range <- range(the.x[, focus.variable], as.matrix(use.condition)[,
        focus.variable])
    tx.derived.range <- f.relationship(x.derived.range, subscript.relationship(transformation.x,
        focus.variable))
    txvec <- seq(tx.derived.range[1], tx.derived.range[2], length = nxpoints)
    focus.untran.x <- f.relationshipinv(txvec, subscript.relationship(transformation.x,
        focus.variable))
    untran.xvalues <- matrix(as.matrix(use.condition), nrow = length(focus.untran.x),
        ncol = ncol(as.matrix(use.condition)), byrow = T)
    untran.xvalues[, focus.variable] <- focus.untran.x
    the.model <- list(distribution = distribution, transformation.x = transformation.x,
        transformation.time = transformation.time, transformation.response = transformation.response)
    Dummy.Dest.Degrad.out <- list(dummy = T, origparam = ADDT.plan.values$theta.vec,
        origparamvcv = diag(length(ADDT.plan.values$theta.vec)),
        model = the.model)
    oldClass(Dummy.Dest.Degrad.out) <- "gmle.out"
    true.results <- fx.ADDT.life.quantile(theta.hat = Dummy.Dest.Degrad.out$origparam,
        p = quantile.list[1], distribution = Dummy.Dest.Degrad.out$model$distribution,
        FailLevel = FailLevel, xuse = untran.xvalues, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
        transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
        transformation.time = Dummy.Dest.Degrad.out$model$transformation.time)
    uber.results.object <- matrix(results.object[1:nrow(results.object),
        1:ncol(results.object), drop = FALSE], ncol = ncol(results.object),
        nrow = nrow(results.object), byrow = F)
    response.mat <- apply(uber.results.object[, 1:length(ADDT.plan.values$theta.vec),
        drop = F], 1, fx.ADDT.life.quantile, p = quantile.list[1],
        distribution = Dummy.Dest.Degrad.out$model$distribution,
        FailLevel = FailLevel, xuse = untran.xvalues, transformation.response = Dummy.Dest.Degrad.out$model$transformation.response,
        transformation.x = Dummy.Dest.Degrad.out$model$transformation.x,
        transformation.time = Dummy.Dest.Degrad.out$model$transformation.time)
    all.eq.zero <- apply(response.mat, 2, function(x) {
        all(x == 0)
    })
    if (any(all.eq.zero)) {
        columns.all.eq.zero <- (1:length(all.eq.zero))[all.eq.zero]
        warn.text <- paste("\nFraction of times for which the estimate \nof the ",
            quantile.list[1], "quantile is 0 is", length(columns.all.eq.zero)/ncol(response.mat),
            "\n")
        cat(warn.text)
        response.mat <- response.mat[, !all.eq.zero]
        if (length(response.mat) == 0) {
            warning(paste(warn.text, "\nNothing to plot"))
        }
    }
    numplotsim <- min(ncol(response.mat), numplotsim)
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- x.derived.range[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(response.mat)[yrna]
    if (map.SMRDDebugLevel() >= 5) {
        cat("\nbefore plot\n")
        browser()
    }
    at.model.plot(x.axis = subscript.relationship(transformation.x,
        focus.variable), y.axis = y.axis, ylim = ylim,
        xlim = xlim, my.title = my.title, title.option = title.option,
        cex = cex, xlab = xlab, ylab = ylab, grids = grids, response.on.yaxis = response.on.yaxis)
    take.out <- c(1, 2, length(txvec) - 1, length(txvec))
    lines(txvec, logb(true.results), col = 1, lwd = 4, lty = 1)
    matlines(txvec[-take.out], logb(response.mat[-take.out, 1:numplotsim]),
        col = 1, lty = 2)
    invisible()
}
