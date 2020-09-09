#' Perform a life test simulation using planning values
#'
#' @param plan.values 
#' @param n 
#' @param censor.time 
#' @param censor.number 
#' @param my.title 
#' @param number.sim 
#' @param quantile.mark 
#' @param number.points 
#' @param perc.low 
#' @param perc.high 
#' @param cex 
#' @param xlim 
#' @param ylim 
#' @param title.option 
#' @param xlab 
#' @param print.bad.count 
#' @param max.lines.to.plot 
#' @param save.data.list 
#' @param number.detail 
#' @param force.from.gui 
#' @param conf.level 
#' @param show.histogram 
#' @param band.method 
#' @param mono.tran 
#' @param ... 
#'
#' @return A series of plots
#' @export
#'
#' @examples
#' \dontrun{
#' plan.values1 <- get.plan.values("Weibull", 
#'                                  beta = 2, 
#'                                  prob = .1, 
#'                                  time = 100, 
#'                                  time.units = "Hours")
#'                                  
#' life.test.simulation(plan.values1, 
#'                      n = 50,
#'                      censor.time = 120, 
#'                      number.detail = 5, 
#'                      quantile.mark = 0.2) 
#'                      
#' plan.values2 <- get.plan.values("Lognormal", 
#'                                 sigma = 0.5,
#'                                 prob = 0.1, 
#'                                 time = 100, 
#'                                 time.units = "Hours")
#' 
#' life.test.simulation(plan.values2, 
#'                      n = 50,
#'                      censor.time = 1000, 
#'                      quantile.mark = .1)
#' }
life.test.simulation <-
function (plan.values, 
          n, 
          censor.time = NULL, 
          censor.number = NULL,
          my.title, 
          number.sim = 2000, 
          quantile.mark = NULL, 
          number.points = 20,
          perc.low = 0.005, 
          perc.high = 0.995, 
          cex = 1, xlim = c(NA,NA), 
          ylim = NULL, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          xlab = plan.values$time.units,
          print.bad.count = T, 
          max.lines.to.plot = min(50, number.sim),
          save.data.list = NULL, 
          number.detail = 5, 
          force.from.gui = F,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          show.histogram = TRUE,
          band.method = GetSMRDDefault("SMRD.ConfidenceBandMethod"),
          mono.tran = FALSE, ...)
{
    sample.size <- n
    if (number.sim <= 0) stop(paste("Requested number of simulations is", number.sim))
    max.lines.to.plot <- max(min(max.lines.to.plot, number.sim), 0)
    number.detail <- max(min(max.lines.to.plot, number.detail), 0)
    if (is.sv3()) sys.calls.index <- 5
    else sys.calls.index <- 2
    save.par <- par(err = -1)
    on.exit(par(save.par))
    if (!is.R()) {
      
        sys.calls.index <- 3
        `if`(any(regexpr("^menu.life.test", as.character(sys.calls()[[sys.calls.index]])) > 0) || force.from.gui,
             from.gui <- T,
             from.gui <- F)
        
      } else {
    
       from.gui <- F
       
      }
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    mu <- plan.values$mu
    sigma <- plan.values$sigma
    distribution <- plan.values$distribution
    if (!is.null(quantile.mark)) {
        zquant <- quant(quantile.mark, distribution)
    }
    mu.save <- rep(NA, number.sim)
    Rfactor <- rep(NA, number.sim)
    vcv.save <- matrix(NA, ncol = 3, nrow = number.sim)
    sigma.save <- rep(NA, number.sim)
    if (is.null(xlab)) xlab <- "Time"
    if (!is.null(censor.time)) {
      
        `if`(is.logdist(distribution),
             the.time <- log(censor.time),
             the.time <- censor.time)
      
        pfail <- wqmf.phibf((the.time - plan.values$mu) / plan.values$sigma, distribution)
        censor.rule.string <- paste("censored at", 
                                    censor.time,
                                    plan.values$time.units)
        expected.failing.string <- paste("with E(r)=", format(n * pfail))
        
  } else {
    
        censor.rule.string <- ""
        expected.failing.string <- paste("with", censor.number, "failing")
        
    }
    if (missing(my.title)) {
        if (numdist(distribution) == 2) {
            my.title.first <- paste("Simulated life test with sample size  =",
                sample.size, "\n", distribution, "Distribution with eta=",
                format(exp(mu)), " and beta=", format(1/sigma))
            my.title <- paste(number.sim, "simulated life tests with sample size = ",
                sample.size, censor.rule.string, "\n", distribution,
                "Distribution with eta=", format(exp(mu)), " and beta=",
                format(1/sigma), expected.failing.string)
      } else {
            my.title.first <- paste("Simulated life test with sampel size = ",
                sample.size, censor.rule.string, "\n", distribution,
                "Distribution with mu=", format(mu), " and sigma=",
                format(sigma))
            my.title <- paste(number.sim, "simulated life tests with sample size =",
                sample.size, "\n", distribution, "Distribution with mu=",
                format(mu), " and sigma=", format(sigma), expected.failing.string)
        }
    }
    if (sample.size > 100) {
        perc.low <- 0.5/sample.size
        perc.high <- 1 - perc.low
    }
    logtime.lower <- mu + sigma * quant(perc.low, distribution)
    logtime.upper <- mu + sigma * quant(perc.high, distribution)
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- c(logtime.lower, logtime.upper)[xrna]
    logtime <- seq(logtime.lower, logtime.upper, length = number.points)
    if (is.logdist(distribution)) {
        if (any(xrna))
            xlim[xrna] <- exp(c(logtime.lower, logtime.upper))[xrna]
        realtime <- exp(logtime)
        if (!is.null(censor.time)) censor.time <- logb(censor.time)
  } else {
        if (is.null(xlim)) xlim <- c(logtime.lower, logtime.upper)
        realtime <- logtime
    }
    ylim <- c(perc.low, perc.high)
    rel.test.plot.setup <- function(logtime, mu, sigma, censor.time,
        perc.high, distribution, xlim, ylim, my.title = my.title,
        cex, xlab = xlab, title.option = title.option, ...) {
        log.of.data <- probplot.setup(distribution, xlim,
            ylim, my.title = my.title, sub.title = NULL, cex = cex,
            grids = F, linear.axes = F, xlab = xlab, title.option = title.option,
            slope.axis = F, ...)
        old.options <- options(digits = 3)
        pop.cdf1 <- wqmf.phibf((logtime - mu)/sigma, distribution)
        lines(logtime, quant(pop.cdf1, distribution), type = "l",
            lwd = 5, col = 6)
        if (!is.null(censor.time)) lines(c(censor.time, censor.time), 
                                         c(y.loc(0), y.loc(1)),
                                         col = 1, 
                                         lwd = 2, 
                                         lty = 1)
        
        if (!is.null(censor.time)) axis(side = 3, 
                                        at = censor.time, 
                                        labels = FALSE, 
                                        adj = 1.0, 
                                        col = 1, 
                                        lwd = 2, 
                                        tck = -.06)  
        mtext(side = 3, expression(Censor~Time%->%''), at = censor.time, adj = 1, line = .15)  
    }
    if (sample.size == 0) return(invisible())
    strip.away <- c(1, 2, number.points - 1, number.points)
    simple.sim.dat <- function(mu, sigma, sample.size, distribution,
        censor.time, censor.number) {
        y.start <- mu + sigma * quant(runif(sample.size), distribution)
        if (is.null(censor.time)) {
          
            `if`(is.null(censor.number),
                 censor.time <- 2 * max(y.start),
                 censor.time <- sort(y.start)[censor.number])
          
        }
        y.final <- y.start[y.start <= censor.time]
        number.censored <- length(y.start) - length(y.final)
        if (number.censored > 0) {
            y.final <- c(y.final, censor.time)
            the.case.weights <- c(rep(1, length(y.final) - 1), number.censored)
            the.censor.codes <- c(rep(1, length(y.final) - 1), 2)
      } else {
            y.final <- c(y.final)
            the.case.weights <- NULL
            the.censor.codes <- NULL
        }
        if (is.logdist(distribution)) {
            y.final <- exp(y.final)
            log.y <- y.final
      } else log.y <- y.final
        data.title <- paste("Simulated", 
                            distribution, 
                            "data mu=",
                            format(mu), 
                            "sigma=", 
                            format(sigma))
        true.model <- list(distribution = distribution, 
                           mu = mu,
                           sigma = sigma)
        if (is.null(the.censor.codes)) {
          
            the.frame <- data.frame(Time = y.final)
            
            sim.ld <- frame.to.ld(the.frame, 
                                  response.column = "Time",
                                  data.title = data.title)
      } else {
            the.frame <- data.frame(Time = y.final, 
                                    Censor = the.censor.codes,
                                    Weights = the.case.weights)
            sim.ld <- frame.to.ld(the.frame, 
                                  response.column = "Time",
                                  censor.column = "Censor", 
                                  case.weight.column = "Weights",
                                  data.title = data.title)
            
            if (map.SMRDDebugLevel() >= 4) cat("\nNumber of failures=", 
                                               length(y.final) - 1, "\n")
        }
        attr(sim.ld, "true.model") <- true.model
        return(sim.ld)
    }
    if (number.detail > 0) {
        for (i in 1:number.detail) {
            sim.ld <- simple.sim.dat(mu, sigma, sample.size,
                distribution, censor.time, censor.number)
            if (is.onlist(i, save.data.list) || map.SMRDDebugLevel() >=
                4) {
                data.file.name <- paste("tmpsim", i, ".ld", sep = "")
                cat(paste("Saving", data.file.name, " \n "))
                assign(envir = .frame0, inherits = !TRUE,data.file.name, sim.ld)
            }
            if (length(case.weights(sim.ld)) > 1) {
                if (is.R())
                  title.line.adj = -3
                else title.line.adj = -2
                if (i <= number.detail) {
                  log.of.data <- rel.test.plot.setup(logtime,
                    mu, sigma, censor.time, perc.high, distribution,
                    xlim, ylim, my.title = my.title.first,
                    cex, xlab = xlab, title.option = title.option,
                    ...)
                   mleprobplot(sim.ld, distribution = distribution,
                    time.range = range(realtime), add = T, band.method = band.method,
                    mono.tran = mono.tran)

                  ml.out <- mlest(sim.ld, distribution = distribution)

                  if (pause(skip = from.gui)) {
                  }
              } else {
                  ml.out <- mlest(sim.ld, distribution = distribution)
                }
                mu.save[i] <- ml.out$theta.hat[1]
                sigma.save[i] <- ml.out$theta.hat[2]
                vcv.save[i, 1] <- ml.out$vcv.matrix[1, 1]
                vcv.save[i, 2] <- ml.out$vcv.matrix[1, 2]
                vcv.save[i, 3] <- ml.out$vcv.matrix[2, 2]
                if (!is.null(quantile.mark)) {
                  Rfactor[i] <- zvalue * sqrt(vcv.save[i, 1] +
                    2 * zquant * vcv.save[i, 2] + vcv.save[i,
                    3] * zquant^2)
                  if (is.logdist(distribution))
                    Rfactor[i] <- exp(Rfactor[i])
                }
            }
        }
    }
    number.fast.sim <- number.sim - number.detail
    if (number.fast.sim > 0) {
        if (is.null(censor.time)) {
            censor.type <- "Type 2"
            if (is.null(censor.number))
                censor.number <- sample.size
            SingleDistSim.out <- SingleDistSim(number.sim = number.fast.sim,
                distribution = distribution, sample.size = sample.size,
                censor.type = censor.type, fail.number = censor.number,
                theta = c(mu, sigma))
      } else {
            censor.type <- "Type 1"
            fail.fraction <- wqmf.phibf((censor.time - mu)/sigma,
                distribution)
            SingleDistSim.out <- SingleDistSim(number.sim = number.fast.sim,
                distribution = distribution, sample.size = sample.size,
                censor.type = censor.type, fail.fraction = fail.fraction,
                theta = c(mu, sigma))
        }
        replace.indices <- (number.detail + 1):number.sim
        mu.save[replace.indices] <- SingleDistSim.out$theta.hat[,
            1]
        sigma.save[replace.indices] <- SingleDistSim.out$theta.hat[,
            2]
        vcv.save[replace.indices, ] <- SingleDistSim.out$vcvobs
        if (!is.null(quantile.mark)) {
            Rfactor[replace.indices] <- zvalue * sqrt(vcv.save[replace.indices,
                1] + 2 * zquant * vcv.save[replace.indices, 2] +
                vcv.save[replace.indices, 3] * zquant^2)
        }
        if (is.logdist(distribution))
            Rfactor[replace.indices] <- exp(Rfactor[replace.indices])
    }
    good.stuff <- !is.na(mu.save)
    log.of.data <- rel.test.plot.setup(logtime, mu, sigma, censor.time,
        perc.high, distribution, xlim, ylim, my.title = my.title,
        cex, xlab = xlab, title.option = title.option, ...)
    zero.failures <- is.na(mu.save)
    for (i in 1:max.lines.to.plot) {
        if (good.stuff[i]) {
            sim.cdf <- wqmf.phibf((logtime - mu.save[i])/sigma.save[i],
                distribution)
            lines(logtime[-strip.away], quant(sim.cdf[-strip.away],
                distribution), type = "l")
        }
    }
    count.bad <- sum(as.numeric(zero.failures))
    if (count.bad > 0 && print.bad.count) {
        text(x.loc(0.77), y.loc(0.2), paste(count.bad, "samples out of",
            number.sim, "with 0 failures"))
    }
    if (!is.null(quantile.mark)) {
        abline(h = quant(quantile.mark, distribution), lty = 1,
            col = 4)
        if (count.bad > 0)
            extra <- "Conditional "
        else extra <- ""
        if (is.logdist(distribution)) {
            extra <- paste(extra, "geometric average ", percent.conf.level(conf.level),
                " confidence \ninterval precision factor R ",
                sep = "")
            if (!is.null(quantile.mark)) {
                Rfactav <- exp(mean(logb(Rfactor[good.stuff])))
                text(x.loc(0.72), y.loc(0.1), paste(extra, "for t_",
                  quantile.mark, " = ", format(Rfactav), sep = ""))
            }
      } else {
            extra <- paste(extra, "average", percent.conf.level(conf.level),
                " confidence interval \nhalf-width D ")
            if (!is.null(quantile.mark)) {
                Rfactav <- mean(Rfactor[good.stuff])
                text(x.loc(0.75), y.loc(0.1), paste(extra, "for y_",
                  quantile.mark, " = ", format(Rfactav), sep = ""))
            }
        }
        wildRfactor <- max(Rfactor[good.stuff])/median(Rfactor[good.stuff])
        if (map.SMRDDebugLevel() >= 4)
            cat("wildRfactor = ", wildRfactor, "\n")
        if (show.histogram) {
            pause(skip = from.gui)
            if (wildRfactor < 50) {
                hist(Rfactor[good.stuff], xlab = paste("R-factor for the",
                  quantile.mark, "quantile"), col = 4, main = "")
          } else {
                hist(log10(Rfactor[good.stuff]), xlab = paste("log (base 10) R-factor for the",
                  quantile.mark, "quantile"), col = 4)
            }
        }
    }
    invisible(list(mu = mu.save[good.stuff], sigma = sigma.save[good.stuff],
        vcv = vcv.save[good.stuff, ], Rfactor = Rfactor[good.stuff]))
}
