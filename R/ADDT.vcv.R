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
ADDT.vcv <-
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

    frame.type <- attr(ADDT.test.plan, "frame.type")
    
    switch(frame.type, 
       vframe = {
         ADDT.test.plan <- ADDT.test.plan
    }, hframe = {
        ADDT.test.plan <- hframe.to.vframe(ADDT.test.plan)
    }, {
        stop(paste("Bad frame.type,type=", frame.type))
    })
    
    `if`(is.null(ADDT.plan.values$theta.vec.cr) & is.null(ADDT.plan.values$beta.cr) & is.null(ADDT.plan.values$sigma.cr),
         competing.risk <- F,
         competing.risk <- T)
    
    levels <- AT.levels(ADDT.test.plan)
    number.accelerators <- ncol(levels)
    beta <- ADDT.plan.values$theta.vec
    number.parameters <- 3 + number.accelerators
    
    if (number.parameters != length(beta)) stop("3+length(number.accelerators)!=length(beta)")
    
    beta0 <- beta[1]
    beta1 <- beta[2]
    beta2.names <- paste("beta", 
                         seq(2, length(ADDT.plan.values$accelvar.units) + 1), sep = "")
    beta2.vec <- ADDT.plan.values$theta.vec[beta2.names]
    sigma <- beta[length(beta)]
    
    if (competing.risk) {
      
        beta.cr <- ADDT.plan.values$theta.vec.cr
        if (number.parameters != length(beta.cr)) stop("3+length(number.accelerators)!=length(beta.cr)")
        beta0.cr <- beta.cr[1]
        beta1.cr <- beta.cr[2]
        beta2.names.cr <- paste(beta2.names, ".cr", sep = "")
        beta2.vec.cr <- ADDT.plan.values$theta.vec.cr[beta2.names.cr]
        sigma.cr <- beta.cr[length(beta.cr)]
        
    }
    
    transformation.x <- fix.inverse.relationship(ADDT.plan.values$transformation.x)
    
    if (length(transformation.x) != number.accelerators){
      
        stop(paste("length(transformation.x)=", " != number.accelerators=",
            number.accelerators, collapse = ","))
      
    }
    
    the.allocations <- allocation(ADDT.test.plan)[, 1]
    xbar <- rep(NA, number.accelerators)
    x.tran <- levels
    
    for (i in 1:number.accelerators) {
      
        x.tran[, i] <- multiple.f.relationship(levels[, i], 
                                               subscript.relationship(transformation.x, i))
        xbar[i] <- wmean(x.tran[, i], the.allocations)
    }
    
    the.times <- times(ADDT.test.plan)
    time.tran <- f.relationship(the.times, ADDT.plan.values$transformation.time)
    tbar <- wmean(time.tran, the.allocations)
    fisher <- matrix(0, number.parameters, number.parameters)
    fisher.orig <- fisher
    ADDT.model <- pseudo.model(ADDT.plan.values, ADDT.test.plan)
    gamma <- f.ADDT.stableparam(ADDT.plan.values$theta.vec, ADDT.model)
    param.names <- names(gamma)
    gamma1 <- gamma[2]
    gamma2.names <- paste("gamma", 
                          seq(2, number.accelerators + 1), sep = "")
    gamma2.vec <- gamma[gamma2.names]
    sigma <- ADDT.plan.values$theta.vec[length(ADDT.plan.values$theta.vec)]
    
    for (i in 1:nrow(ADDT.test.plan)) {
      
        if (competing.risk) {
          
            mu.cr <- beta0.cr + beta1.cr * 
                      exp(sum(beta2.vec.cr * x.tran[i, , drop = F])) * time.tran[i, 1]
            mu <- beta0 + beta1 * 
                   exp(sum(beta2.vec * x.tran[i, , drop = F])) * time.tran[i, 1]
            xi <- (mu.cr - mu) / sigma
            lsinf.out <- lsinf(xi, "right", ADDT.plan.values$distribution)
            
      } else { 
        
        lsinf.out <- lsinf(1e+35, "uncensored", ADDT.plan.values$distribution)
        
      }
        fishersub <- matrix(c(lsinf.out$f11, lsinf.out$f12 *
            sigma, lsinf.out$f12 * sigma, lsinf.out$f22 * sigma^2),
            2, 2)
        dD.dgamma0 <- 1
        gamma.x <- sum(beta2.vec * (x.tran[i, , drop = F] - xbar))
        dD.dgamma1 <- exp(gamma.x) * time.tran[i, 1] - tbar

            dD.dgamma2 <- (gamma1 * as.numeric(time.tran[i, 1,
                drop = F])) %*% (as.matrix(x.tran[i, , drop = F]) -
                xbar) * exp(gamma.x)

        grad <- c(dD.dgamma0, dD.dgamma1, dD.dgamma2)
        M <- cbind(c(grad, 0), c(rep(0, length(grad)), 1))
        fisheri <- M %*% fishersub %*% t(M)
        fisher <- fisher + the.allocations[i] * fisheri
    }
    dimnames(fisher) <- list(param.names, param.names)
    the.tran.vcv <- my.solve(fisher/sigma^2)
    if (attr(the.tran.vcv, "error") > 0) warning("Probably indicates a poor test plan")
    the.orig.vcv <- f.analorigparmvcv(the.tran.vcv, 
                                      beta, 
                                      gamma1,
                                      gamma2.vec, 
                                      tbar, 
                                      xbar)
    
    return(list(the.tran.fim = fisher/sigma^2, 
                the.orig.vcv = the.orig.vcv,
                the.tran.vcv = the.tran.vcv))
}
