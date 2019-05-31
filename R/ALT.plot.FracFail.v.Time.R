#' Title
#'
#' @param results.object 
#' @param x.of.interest 
#' @param plan.values.string 
#' @param plan.string 
#' @param quantile.range 
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
#' @param how.time.range 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' NelsonInsulation.Weibull.altpv  <- 
#'   get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
#'                                            slope= c(-12.28,-1.296),
#'                                            relationship = c("log","log"),
#'                                            accelvar.units = c("vpm","cm"),
#'                                            time.units = "Hours", 
#'                                            censor.time = 1000, probs = c(1.8e-6),
#'                                            accelvar = c(80,.266), 
#'                                            beta = 1/.6734, 
#'                                            use.conditions = c(80,.266))
#' 
#' print(NelsonInsulation.Weibull.altpv)
#' 
#' ##	define two-variable ALT test plans
#' 
#' NelsonInsulation.altplan <- 
#'   get.alt.test.plan.direct(accel.variable.levels = cbind(c(120,120,120,150,150,150,175,175,175,200,200,200),
#'                                                          c(.163,.266,.355,.163,.266,.355,.163,.266,.355,.163,.266,.355)),
#'                            number.of.units = c(11,18,11,8,14,8,8,14,8,11,18,11),
#'                            censor.times = rep(1000,12),
#'                            accelvar.names = c("Volts per mm","Thick"),
#'                            describe.string = "NelsonInsulation Factorial Plan")
#' 
#' print(NelsonInsulation.altplan)
#' print(NelsonInsulation.Weibull.altpv)
#' 
#' ##	compute the fisher and vcv matrices
#' 
#' ALT.vcv(NelsonInsulation.altplan,
#'         NelsonInsulation.Weibull.altpv)
#' 
#' ##	compute the large-sample approximate precision (R) factors
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          quantile.of.interest = c(.1,.5))
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          use.conditions = c(175,.163),
#'          quantile.of.interest = c(.1,.5))
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          use.conditions = c(100,.1),
#'          quantile.of.interest = c(.1,.5))
#' 
#' ##	sample size needed for a given value of R
#' 
#' plot.alt.sample.size(NelsonInsulation.altplan,
#'                      NelsonInsulation.Weibull.altpv)
#' 
#' 
#' NelsonInsulation.sim.out <- ALTsim(NelsonInsulation.altplan, 
#'                                    NelsonInsulation.Weibull.altpv, number.sim = 400,
#'                                    show.detail.on = 1)
#' 
#' ALT.plot.time.v.x(NelsonInsulation.sim.out)
#' 
#' ALT.plot.time.v.x(NelsonInsulation.sim.out,
#'                   x.of.interest = c(100,.1),
#'                   xlim = c(100,200))
#' 
#' ALT.plot.FracFail.v.Time(NelsonInsulation.sim.out)
#' 
#' ALT.plot.FracFail.v.Time(NelsonInsulation.sim.out,x.of.interest = c(25,10), 
#'                          xlim = c(100,10000))
#' 
#' summarize.simultation.results(NelsonInsulation.sim.out, 
#'                               "Joint and Marginal", 
#'                               focus.quantity1 = "quantile",
#'                               focus.quantity.detail1 = 0.1,
#'                               x.of.interest1 = "100;.1",
#'                               focus.quantity2 = "parameter",
#'                               focus.quantity.detail2 = 3,
#'                               x.of.interest2 = NA,
#'                               plot.type = "density")
#' 
#' 
#' }
ALT.plot.FracFail.v.Time <-
function (results.object, 
          x.of.interest = NULL,
          plan.values.string = NULL,
          plan.string = NULL, 
          quantile.range = c(0.01, 0.99), 
          ylim = c(NA,NA), 
          xlim = c(NA, NA), 
          xlab = NULL, 
          ylab = NULL, 
          my.title = NULL,
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          grids = F, 
          numplotsim = 50, 
          nxpoints = 50,
          cex = 1, 
          how.time.range = "modified test plan")
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

    use.conditions <- x.of.interest
    number.sim <- nrow(results.object)
    
    if (is.null(use.conditions)) {
      
        use.conditions <- attr(results.object, "use.conditions")
        if (is.null(use.conditions)) use.conditions <- attr(results.object, "plan.values")$use.conditions
        if (is.null(use.conditions)) stop("Use conditions not specified")
        
    }
    
    if (is.character(use.conditions)) {
      
        use.conditions <- string.to.frame(use.conditions)
        
  } else {
    
        if (is.numeric(use.conditions) && !is.data.frame(use.conditions)) {
          
            use.conditions <- as.data.frame(matrix(use.conditions,
                                                   ncol = length(use.conditions)))
                                                   
        }
    
  }
            
    character.use.conditions <- as.character(use.conditions)
    ALT.plan.values <- attr(results.object, "plan.values")
    ALT.test.plan <- attr(results.object, "plan")
    par(err = -1)
    
    if (is.null(plan.string)) plan.string <- attr(results.object, "plan.string")
    if (is.null(plan.values.string)) plan.values.string <- attr(results.object, "plan.values.string")
    if (is.null(xlab)) xlab <- get.time.units(ALT.plan.values)
    if (is.null(ylab)) ylab <- GetSMRDDefault("SMRD.LabelOnYaxis")
    distribution <- ALT.plan.values$distribution
    orig.relationship <- ALT.plan.values$relationship
    
    model.string <- paste("x:", 
                          paste(ALT.plan.values$relationship,
                                character.use.conditions, collapse = ","), 
                          ", Dist:",distribution, sep = "")
    
    if (is.null(my.title)) {
      
        my.title <- paste("Accelerated life test simulation based on\n",
            plan.string, plan.values.string, "\nFraction failing versus",
            xlab, "\n", model.string)
        
    }
    relationship <- fix.inverse.relationship(orig.relationship)
    slope.name <- attr(orig.relationship, "slope.name")
    the.xmat <- AT.levels(ALT.test.plan)
    numplotsim <- min(number.sim, numplotsim)
    x.tran <- as.matrix(use.conditions)
    
    for (i in 1:ncol(x.tran)) {
      
        x.tran[, i] <- f.relationship(use.conditions[, i], 
                                      subscript.relationship(orig.relationship, i))
        
    }
    
    plan.time.range <- range(ALT.test.plan[, "censor.times"])
    full.dist.time.range <- range(ALT.life.quantile(theta.hat = ALT.plan.values$theta.vec,
                                                    p = quantile.range[1], 
                                                    distribution = distribution, 
                                                    xuse.tran = x.tran),
                                  ALT.life.quantile(theta.hat = ALT.plan.values$theta.vec,
                                                    p = quantile.range[2], 
                                                    distribution = distribution,
                                                    xuse.tran = x.tran))
    
    ALT.life.failure.probability(theta.hat = ALT.plan.values$theta.vec,
                                 time.vec = full.dist.time.range, 
                                 distribution = distribution,
                                 xuse.tran = x.tran)
    
    switch(how.time.range, `test plan` = {
        derived.time.range <- plan.time.range
    }, `entire distribution` = {
        derived.time.range <- full.dist.time.range
    }, `modified test plan` = {
        if (plan.time.range[1] < full.dist.time.range[1] && full.dist.time.range[1] >
            plan.time.range[2]) derived.time.range <- c(plan.time.range[1],
            full.dist.time.range[1]) else derived.time.range <- full.dist.time.range
    })
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- derived.time.range[xrna]
    if (xlim[1] <= 0) xlim[1] <- 0.001
    time.vec <- log.seq(xlim[1], xlim[2], length = nxpoints)
    
    frac.fail.true <- ALT.life.failure.probability(theta.hat = ALT.plan.values$theta.vec,
                                                   time.vec = time.vec, 
                                                   distribution = ALT.plan.values$distribution,
                                                   xuse.tran = x.tran)
    
    uber.results.object <- matrix(results.object[1:nrow(results.object),1:ncol(results.object), drop = FALSE], 
                                  ncol = ncol(results.object),
                                  nrow = nrow(results.object), 
                                  byrow = F)
    
    frac.fail.mat <- apply(uber.results.object[, 1:length(ALT.plan.values$theta.vec), drop = F], 
                           1, 
                           ALT.life.failure.probability, 
                           time.vec = time.vec,
                           distribution = ALT.plan.values$distribution, 
                           xuse.tran = x.tran)
    
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(frac.fail.mat)[yrna]
    probplot.setup(distribution, 
                   ylim = ylim, 
                   xlim = xlim,
                   my.title = my.title, 
                   title.option = title.option, 
                   cex = cex,
                   xlab = xlab, 
                   ylab = ylab, 
                   grids = grids)
    
    take.out <- c(1, 2, length(time.vec) - 1, length(time.vec))
    lines(logb(time.vec), 
          pp.quant(frac.fail.true, distribution),
          col = 1, 
          lwd = 4, 
          lty = 1)
    
    matlines(logb(time.vec[-take.out]), 
             pp.quant(frac.fail.mat[-take.out,1:numplotsim], distribution), 
             col = 1,
             lty = 2)
    
    invisible()
}
