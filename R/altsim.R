#' Simulate Accelerated Life Test Data
#' 
#' @description Simulates data from an accelerated life test
#' 
#' @name altsim
#' 
#' @rdname altsim_r
#' 
#' @param accel.var.mat 
#' @param nsamsz 
#' @param centim 
#' @param theta 
#' @param distribution 
#' @param number.sim 
#' @param kctype 
#' @param escale 
#' @param e 
#' @param parameter.fixed 
#' @param intercept 
#' @param kprint 
#' @param maxit 
#' @param debug1 
#' @param randomize 
#'
#' @return List of plan values and simulated data
#' @export
#' @examples 
#' \dontrun{
#' 
#' test.matrix <- cbind(V1 = c(1,1,2,2),V2 = c(1,2,1,2))
#' 
#' altsim.out <- altsim(accel.var.mat = test.matrix, 
#'                      nsamsz = c(4,4,4,4), 
#'                      centim = c(40000,40000,40000,40000), 
#'                      theta = c(1,-24,5,1), 
#'                      distribution = "normal",
#'                      number.sim = 10, 
#'                      debug = F)
#' 
#' altsim.out <- altsim(accel.var.mat = as.matrix(c(1,2,3,4)), 
#'                      nsamsz = c(4,4,4,4), 
#'                      centim = c(200,200,200,200), 
#'                      theta = c(1,-2,3), 
#'                      distribution = "normal", 
#'                      number.sim = 5, 
#'                      debug = F, 
#'                      kprint = 0)
#' 
#' altsimReturnFrame(accel.var.mat = test.matrix, 
#'                   nsamsz = c(4,4,4,4),
#'                   centim = c(100,100,100,100),
#'                   theta = c(1,1,1,2),
#'                   distribution =" lognormal", 
#'                   relationship = c("linear","log"))
#' 
#' ## Simulate a single ALT data set
#' 
#' altsimReturnFrame(accel.var.mat = test.matrix, 
#'                   nsamsz = c(4,4,4,4),
#'                   centim = c(100,100,100,100), 
#'                   theta = c(1,1,1,2), 
#'                   distribution = "lognormal", 
#'                   relationship = c("linear","log"))
#' 
#' }
altsim <- function (accel.var.mat, 
          nsamsz, 
          centim, 
          theta, 
          distribution,
          number.sim, 
          kctype = 1, 
          escale = 10000, 
          e = rep(1e-04, number.parameters),
          parameter.fixed = rep(F, number.parameters),
          intercept = T,
          kprint = 0, 
          maxit = 500,
          debug1 = F, 
          randomize = T)
{
    number.cases <- sum(nsamsz + 1)
    plan <- list(accel.var.mat = accel.var.mat, 
                 nsamsz = nsamsz,
                 centim = centim)
    nty <- 0
    `if`(intercept, int <- 1, int <- 0)
    theta.hat <- theta
    distribution.number <- numdist(distribution)
    if (is.null(accel.var.mat)) {
        accel.var.mat <- 0
        if (int != 1) stop("must have int=1 if no x matrix")
        param.names <- c("mu", "sigma")
        nter <- 1
        nsubex <- 1
        nacvar <- 0
  } else {
        nsubex <- nrow(accel.var.mat)
        nacvar <- ncol(accel.var.mat)
        param.names <- c(paste("beta", 0:ncol(accel.var.mat), sep = ""), "sigma")
        nter <- ncol(accel.var.mat) + int
    }
    number.parameters <- nter + 1
    if (generic.distribution(distribution) == "exponential") {
        distribution.number <- 2
        parameter.fixed[number.parameters] <- T
        number.parametersx <- number.parameters - 1
    }
    number.things.returned <- number.parameters + ((number.parameters) *
        (number.parameters + 1))/2 + 2
    y <- matrix(rep(0, number.cases), ncol = 1)
    case.weights <- rep(0, number.cases)
    censor.codes <- rep(0, number.cases)
    ny <- ncol(y)
    xmat <- matrix(1, nrow = number.cases, ncol = nter)
    ndscrat <- 4 * (number.parameters * number.cases + 5 * number.parameters *
        number.parameters + 12 * number.parameters + 1)
    niscrat <- 2 * (number.parameters + 1)
    if(debug1) browser()
    
    zout <- ALTSIM(x = xmat, 
                   y = y,
                   cen = as.integer(censor.codes),
                   wt = as.integer(case.weights),
                   nrow = as.integer(number.cases), 
                   nter = as.integer(nter),
                   ny = as.integer(ny), 
                   nty = as.integer(nty), 
                   ty = matrix(0, nrow = number.cases, ncol = 1),
                   tc = integer(number.cases), 
                   kdist = as.integer(distribution.number),
                   gamthr = double(number.cases), 
                   lfix = as.logical(parameter.fixed),
                   nparm = as.integer(number.parameters), 
                   intcpt = as.integer(int),
                   escale = as.double(escale), 
                   e = as.double(e), 
                   maxit = as.integer(maxit),
                   kprint = as.integer(kprint), 
                   dscrat = double(ndscrat),
                   iscrat = integer(niscrat), 
                   dev = matrix(0,nrow = number.cases, ncol = 3), 
                   thetah = as.double(theta.hat), 
                   fsder = double(number.parameters),
                   vcv = matrix(0,nrow = number.parameters, ncol = number.parameters),
                   r = matrix(0,nrow = number.parameters, ncol = number.parameters),
                   res = matrix(0, ncol = ny, nrow = number.cases), 
                   fv = matrix(0,ncol = ny, nrow = number.cases), 
                   theta = as.double(theta), 
                   xnew = matrix(0, nrow = number.cases, ncol = nter),
                   ynew = matrix(0, nrow = number.cases, ncol = ny), 
                   centim = as.double(centim),
                   acvar = as.matrix(accel.var.mat), 
                   nsubex = as.integer(nsubex),
                   nacvar = as.integer(nacvar), 
                   nsamsz = as.integer(nsamsz),
                   krfail = integer(nsubex),
                   kctype = as.integer(kctype), 
                   retmat = matrix(0, ncol = number.sim, nrow = number.things.returned), 
                   numret = as.integer(number.things.returned),
                   numsim = as.integer(number.sim), 
                   iersim = integer(1))
    
    if (zout$ints$iersim > 0 || debug1) {
      
        browser()
        if (zout$ints$iersim > 0) stop("Need more space for observations")
      
    }
    
    `if`(nter <= 1,
         param.names <- c("mu", "sigma"),
         param.names <- c("b0", paste("b", 1:(nter - 1), sep = ""), "sigma"))
    
    return.matrix <- t(matrix(zout$nummat$retmat, nrow = number.things.returned))
    theta.hat.star <- return.matrix[, 2:(number.parameters + 1), drop = F]
    dimnames(theta.hat.star) <- list(NULL, param.names)
    ierstuff <- floor(return.matrix[, 1] + 0.1)
    vcv <- return.matrix[, (number.parameters + 3):(number.parameters +
        ((number.parameters) * (number.parameters + 1))/2 + 2),
        drop = F]
    dimnames(vcv) <- list(NULL, get.vcv.names(param.names))
    return(list(plan = plan, 
                theta = theta, 
                theta.hat = theta.hat.star,
                vcv = vcv, 
                ierstuff = ierstuff, 
                likelihood = return.matrix[, number.parameters + 2]))
}

#
#

#' Title
#'
#' @name ALTsim
#' 
#' @rdname ALT_sim
#' 
#' @param ALT.test.plan 
#' @param ALT.plan.values 
#' @param number.sim 
#' @param show.detail.on 
#' @param xlim 
#' @param ylim 
#' @param sim.data.title 
#' @param use.conditions 
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
ALTsim <-
  function (ALT.test.plan, ALT.plan.values, number.sim, show.detail.on = 0,
            xlim = c(NA, NA), ylim = c(NA, NA), sim.data.title = NULL,
            use.conditions = NULL)
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
    
    if (is.null(attr(ALT.test.plan, "plan.string")))
      plan.string <- deparse(substitute(ALT.test.plan))
    if (is.null(attr(ALT.plan.values, "plan.values.string")))
      plan.values.string <- deparse(substitute(ALT.plan.values))
    distribution <- ALT.plan.values$distribution
    orig.relationship <- ALT.plan.values$relationship
    relationship <- fix.inverse.relationship(orig.relationship)
    number.parameters <- 2 + length(relationship)
    theta.hat.star <- matrix(NA, nrow = number.sim, ncol = number.parameters)
    vcv <- matrix(NA, nrow = number.sim, ncol = ((number.parameters) *
                                                   (number.parameters + 1))/2)
    likelihood <- rep(NA, length = number.sim)
    ierstuff <- rep(NA, length = number.sim)
    number.of.units <- ALT.test.plan$number.units
    time.units <- ALT.plan.values$time.units
    levels <- as.matrix(AT.levels(ALT.test.plan))
    number.accelerators <- ncol(levels)
    the.allocations <- allocation(ALT.test.plan)[, 1]
    for (j in 1:number.accelerators) {
      levels[, j] <- multiple.f.relationship(levels[, j], subscript.relationship(orig.relationship,
                                                                                  j))
    }
    input.title <- paste(plan.string, plan.values.string)
    if (is.null(sim.data.title))
      sim.data.title <- paste("Simulated data from", input.title)
    theta <- ALT.plan.values$theta.vec
    censor.times <- ALT.test.plan$censor.times
    number.good <- 0
    if (as.numeric(show.detail.on) > 0)
      for (i in 1:as.numeric(show.detail.on)) {
        data.ld <- altsimReturnFrame(accel.var.mat = levels,
                                     nsamsz = number.of.units, centim = censor.times,
                                     theta = theta, distribution = distribution, relationship = orig.relationship,
                                     time.units = time.units, my.title = sim.data.title)
        if (map.SMRDDebugLevel() >= 2)
          print(data.ld)
        vcv[i, ] <- attr(data.ld, "vcv")
        likelihood[i] <- attr(data.ld, "likelihood")
        theta.hat.star[i, ] <- attr(data.ld, "theta.hat")
        ierstuff[i] <- attr(data.ld, "ierstuff")
        groupm.results <- groupm.mleprobplot(data.ld, distribution = distribution,
                                             relationship = orig.relationship, new.data = string.to.frame(paste(use.conditions,
                                                                                                                collapse = ";")))
        print(groupm.results,0)
      }
    if (as.numeric(show.detail.on) > 0) {
      assign(envir = .frame0,  inherits = TRUE,"last.sim.ALT.ld", data.ld)
      cat("The last simulated ALT data set has been saved in last.sim.ALT.ld\n")
    }
    number.sim.remaining <- number.sim - as.numeric(show.detail.on)
    if (number.sim.remaining > 0) {
      altsim.out <- altsim(accel.var.mat = levels, 
                           nsamsz = number.of.units,
                           centim = censor.times, 
                           theta = theta, 
                           distribution = distribution,
                           number.sim = number.sim.remaining,
                           debug1= F, kprint = 0)
      indices.of.new <- (show.detail.on + 1):number.sim
      ierstuff[indices.of.new] <- altsim.out$ierstuff
      vcv[indices.of.new, ] <- altsim.out$vcv
      likelihood[indices.of.new] <- altsim.out$likelihood
      theta.hat.star[indices.of.new, ] <- altsim.out$theta.hat
    }
    good.ones <- ierstuff == 0
    number.good <- sum(as.numeric(good.ones))
    vcv <- vcv[good.ones, , drop = F]
    theta.hat.star <- theta.hat.star[good.ones, , drop = F]
    if (number.good < number.sim) warning(paste("******There were", 
                                                number.sim - number.good,
                                                "bad simulations"))
    if (number.good == 0) stop("No good simulation results returned")
    param.names <- c("beta0", 
                     paste("beta", 1:(length(relationship)), sep = ""), 
                     "sigma")
    dimnames(theta.hat.star) <- list(NULL, param.names)
    dimnames(vcv) <- list(NULL, get.vcv.names(param.names))
    return.list <- cbind(theta.hat.star, vcv)
    attr(return.list, "plan.values") <- ALT.plan.values
    attr(return.list, "plan") <- ALT.test.plan
    attr(return.list, "theta") <- theta
    attr(return.list, "ierstuff") <- ierstuff
    attr(return.list, "likelihood") <- likelihood
    attr(return.list, "plan.string") <- plan.string
    attr(return.list, "plan.values.string") <- plan.values.string
    attr(return.list, "title") <- paste("ALT Simulation Results",
                                        "\nfrom", input.title)
    model <- list(distribution = ALT.plan.values$distribution,
                  relationships = ALT.plan.values$relationship, 
                  explan.vars = attr(ALT.test.plan,"accelvar.names"), 
                  accelvar.units = ALT.plan.values$accelvar.units,
                  param.names = param.names)
    
    attr(return.list, "model") <- model
    oldClass(return.list) <- c("simulate.alt.out", "matrix")
    MysetOldClass(attr(return.list, "class"))
    return(return.list)
  }
