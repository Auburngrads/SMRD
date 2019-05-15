#' Title
#'
#' @param results.object 
#' @param x.of.interest 
#' @param FailLevel 
#' @param plan.values.string 
#' @param plan.string 
#' @param quantile 
#' @param ylim 
#' @param xlim 
#' @param xlab 
#' @param ylab 
#' @param my.title 
#' @param title.option 
#' @param grids 
#' @param numplotsim 
#' @param nxpoints 
#' @param cex 
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
ADDT.plot.Deg.v.Time <-
function (results.object, x.of.interest = NULL, FailLevel = NULL,
    plan.values.string = NULL, plan.string = NULL, quantile = 0.5,
    ylim = c(NA, NA), xlim = c(NA, NA), xlab = NULL, ylab = NULL,
    my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F, numplotsim = 50,
    nxpoints = 50, cex = 1)
{
    use.condition <- x.of.interest
    number.sim <- nrow(results.object)
    if (is.null(use.condition))
        use.condition <- attr(results.object, "use.condition")
    if (is.null(use.condition))
        stop("Use condition not specified")
    if (is.character(use.condition))
        use.condition <- string.to.frame(use.condition)
    if (is.null(FailLevel))
        FailLevel <- attr(results.object, "FailLevel")
    ADDT.plan.values <- attr(results.object, "plan.values")
    ADDT.test.plan <- attr(results.object, "plan")
    if (is.null(plan.string))
        plan.string <- attr(results.object, "plan.string")
    if (is.null(plan.values.string))
        plan.values.string <- attr(results.object, "plan.values.string")
    FailLevelDef <- paste(FailLevel, get.response.units(ADDT.plan.values))
    if (is.null(xlab))
        xlab <- get.time.units(ADDT.plan.values)
    if (is.null(ylab))
        ylab <- get.response.units(ADDT.plan.values)
    distribution <- ADDT.plan.values$distribution
    transformation.x <- ADDT.plan.values$transformation.x
    transformation.time <- ADDT.plan.values$transformation.time
    x.axis <- transformation.time
    transformation.response <- ADDT.plan.values$transformation.response
    y.axis <- transformation.response
    model.string <- paste("Resp:", transformation.response, ",Time:",
        transformation.time, ",x:", paste(ADDT.plan.values$transformation.x,
            collapse = ","), ", Dist:", distribution, sep = "")
    if (is.null(my.title))
        my.title <- paste("Accelerated destructive degradation test simulation based on\n",
            plan.string, plan.values.string, "\n", quantile,
            "quantile of degradation versus", xlab, "at", paste(use.condition,
                ADDT.plan.values$accelvar.units, collapse = ","),
            "\n", model.string)
    transformation.x <- fix.inverse.relationship(transformation.x)
    slope.name <- attr(transformation.x, "slope.name")
    y.axis <- "log"
    numplotsim <- min(number.sim, numplotsim)
    the.model <- list(distribution = distribution, transformation.x = transformation.x,
        transformation.time = transformation.time, transformation.response = transformation.response)
    Dummy.Dest.Degrad.out <- list(dummy = T, origparam = ADDT.plan.values$theta.vec,
        origparamvcv = diag(length(ADDT.plan.values$theta.vec)),
        model = the.model)
    oldClass(Dummy.Dest.Degrad.out) <- "gmle.out"
    derived.time.range <- range(times(ADDT.test.plan))
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- derived.time.range[xrna]
    time.vec <- seq(xlim[1], xlim[2], length = nxpoints)
    degradation.true <- fx.ADDT.degradation.quantile(theta.hat = Dummy.Dest.Degrad.out$origparam,
        p = quantile, time.vec = time.vec, distribution = distribution,
        xuse = use.condition, transformation.x = transformation.x,
        transformation.time = transformation.time)
    uber.results.object <- matrix(results.object[1:nrow(results.object),
        1:ncol(results.object), drop = FALSE], ncol = ncol(results.object),
        nrow = nrow(results.object), byrow = F)
    degradation.mat <- (apply(uber.results.object[, 1:length(ADDT.plan.values$theta.vec),
        drop = F], 1, fx.ADDT.degradation.quantile, p = quantile,
        time.vec = time.vec, distribution = distribution, xuse = use.condition,
        transformation.x = transformation.x, transformation.time = transformation.time))
    derived.ylim <- f.relationshipinv(range(degradation.mat),
        transformation.response)
    yrna <- is.na(ylim)
    trans.time.vec <- f.relationship(time.vec, x.axis)
    if (any(yrna))
        ylim[yrna] <- derived.ylim[yrna]

    plot.paper(ylim = ylim, xlim = xlim, x.axis = x.axis,
        y.axis = y.axis, my.title = "", title.option = title.option,
        cex = cex, xlab = xlab, ylab = ylab, grids = grids, cex.title = 0.8)
    take.out <- c(1, 2, length(time.vec) - 1, length(time.vec))
    lines(trans.time.vec, degradation.true, col = 1, lwd = 4,
        lty = 1)
    matlines(trans.time.vec[-take.out], degradation.mat[-take.out,
        1:numplotsim], col = 1, lty = 2)
    mtext(text = my.title, line = 0.5, side = 3)
    invisible()
}
