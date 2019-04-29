distribution.plot <-
  function (distribution, 
            shape, 
            prob.range = c(0.01, 0.99), 
            number.points = 1000,
            type = "all", 
            axsi = F, 
            scale = rep(1, length(shape)), 
            plot.haz.log = F,
            original.par = T, 
            my.title = NULL, 
            my.subtitle = NULL,
            location = rep(0,length(shape)), 
            char.fudge = "", 
            shape2 = 1, 
            exponential2 = T,...)
  {
    invisible()
    assign(envir = .frame0,  inherits = TRUE,"my.title", my.title)
    assign(envir = .frame0,  inherits = TRUE,"my.subtitle", my.subtitle)
    old.par <- par()
    lwd = 2
    par(err = -1, bg = NA)
    if (original.par)
      on.exit({
        old.par[c("cin", "cra", "csi", "cxy", "din", "page")] <- NULL
        par(old.par)
        par(mfrow = c(1,1))
        par(new = F)
      })
    
    if (is.null(scale)) scale <- rep(1, max(1, length(shape)))
    
    distribution.plot.range <- function(distribution, 
                                        shape,
                                        scale, 
                                        prob.range, 
                                        location, 
                                        shape2) {
      
      switch(generic.distribution(distribution), 
             
         exponential = {
               
         return(c(min(qexp(prob.range[1],     shape)), 
                  max(qexp(prob.range[2], 1 / shape))))
           
      }, normal = {
        
         return(c(min(qnorm(prob.range[1], mean = location, shape)), 
                  max(qnorm(prob.range[2], mean = location, shape))))
        
      }, logistic = {
        
         return(c(min(qlogis(prob.range[1], location = location, shape)), 
                  max(qlogis(prob.range[2], location = location, shape))))
        
      }, lev = {
        
         return(c(min(qlev(prob.range[1], location = location, shape)), 
                  max(qlev(prob.range[2], location = location, shape))))
        
      }, sev = {
        
         return(c(min(qsev(prob.range[1], location = location, shape)), 
                  max(qsev(prob.range[2], location = location, shape))))
        
      }, weibull = {
        
         return(c(min(qweibull(prob.range[1], shape)), 
                  max(qweibull(prob.range[2], shape))))
        
      }, gamma = {
        
         return(c(min(qgamma(prob.range[1], shape)), 
                  max(qgamma(prob.range[2], shape))))
        
      }, igau = {
        
         return(c(min(qigau(prob.range[1], shape)), 
                  max(qigau(prob.range[2], shape))))
        
      }, bisa = {
        
         return(c(min(qbisa(prob.range[1], shape)), 
                  max(qbisa(prob.range[2], shape))))
        
      }, goma = {
        
         return(c(min(qgoma(prob.range[1], shape, shape2)),
                  max(qgoma(prob.range[2], shape, shape2))))
        
      }, lognormal = {
        
         return(c(min(qlnorm(prob.range[1], 0, shape)), 
                  max(qlnorm(prob.range[2], 0, shape))))
        
      }, loglogistic = {
        
         return(c(min(qloglogis(prob.range[1], 0, shape)),
                  max(qloglogis(prob.range[2], 0, shape))))
      })
      
    }
    
    pdf.plot <- function(x, 
                         distribution, 
                         shape, 
                         scale = 1, 
                         location,
                         shape2,...) {
      
      par(mar = c(4.25, 5, 3.75, 2) + 0.1)
      switch(generic.distribution(distribution), 
             
             exponential = {
        matplot(x, y = outer(x, 1/shape, dexp), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, weibull = {
        matplot(x, y = outer(x, shape, dweibull), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, sev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  dsev), lty = 1:length(shape), type = "l", las = 1,
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, lev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  dlev), lty = 1:length(shape), type = "l", las = 1,
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, logistic = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  dlogis), lty = 1:length(shape), type = "l", las = 1,
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, normal = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  dnorm), lty = 1:length(shape), type = "l", las = 1,
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, lognormal = {
        matplot(x, y = dist.outer(x, shape, 0, dlnorm), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, loglogistic = {
        matplot(x, y = dist.outer(x, shape, 0, dloglogis),
                lty = 1:length(shape), type = "l", las = 1, cex = 1.2, cex.axis = 1.1,
                xlab = "", ylab = "", lwd = lwd, ...)
      }, gamma = {
        matplot(x, y = outer(x, shape, dgamma), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, igau = {
        matplot(x, y = outer(x, shape, digau), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, bisa = {
        matplot(x, y = outer(x, shape, dbisa), lty = 1:length(shape),
                type = "l", las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd,
                ...)
      }, goma = {
        matplot(x, y = dist.outer(x, shape, shape2, dgoma),
                lty = 1:length(shape), type = "l", las = 1, cex = 1.2, cex.axis = 1.1,
                xlab = "", ylab = "", lwd = lwd, ...)
      })

      if (is.null(my.subtitle))
        title("Probability Density Function", cex = 1)
      title(xlab = "t", cex.lab = 1.5)
      usr.par <- par("usr")
      xpos <- usr.par[1] - 0.25 * (usr.par[2] - usr.par[1])
      ypos <- usr.par[3] + 0.5 * (usr.par[4] - usr.par[3])
      mtext(side = 2, line = 3, srt = 90, "f(t)", cex = 1.1)
    }
    
    cdf.plot <- function(x, 
                         distribution, 
                         shape, 
                         scale, 
                         location,
                         shape2, ...) {
      
      par(mar = c(4.25, 5, 3.75, 2) + 0.1)
      switch(generic.distribution(distribution), exponential = {
        matplot(x, y = outer(x, 1/shape, pexp), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, weibull = {
        matplot(x, y = outer(x, shape, pweibull), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, sev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  psev), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, lev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  plev), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, logistic = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  plogis), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, normal = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  pnorm), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, lognormal = {
        matplot(x, y = dist.outer(x, shape, 0, plnorm), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, loglogistic = {
        matplot(x, y = dist.outer(x, shape, 0, ploglogis),
                lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, gamma = {
        matplot(x, y = outer(x, shape, pgamma), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, igau = {
        matplot(x, y = outer(x, shape, pigau), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, bisa = {
        matplot(x, y = outer(x, shape, pbisa), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, goma = {
        matplot(x, y = dist.outer(x, shape, shape2, pgoma),
                lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      })
      cdf.ylabs <- c("0", ".5", "1")
      axis(2, at = as.numeric(cdf.ylabs), labels = cdf.ylabs,
           cex = 1.2, las = 1, hadj = .4)
      if (is.null(my.subtitle))
        title("Cumulative Distribution Function", cex = 1)
      title(xlab = "t", cex.lab = 1.5)
      usr.par <- par("usr")
      xpos <- usr.par[1] - 0.2 * (usr.par[2] - usr.par[1])
      ypos <- usr.par[3] + 0.5 * (usr.par[4] - usr.par[3])
      mtext(side = 2, line = 3, srt = 90, "F(t)", cex = 1.1)
    }
    hf.plot <- function(x, distribution, shape, scale = rep(1, length(shape)), 
                        plot.haz.log = F, location, shape2, ...) {
      par(mar = c(4.25, 5, 3.25, 2) + 0.1)
      if (plot.haz.log)
        logger <- "y"
      else logger <- ""
      switch(generic.distribution(distribution), exponential = {
        matplot(x, 
                y = outer(x, 1/shape, dexp)/(outer(1/x,1/shape, pexp)), 
                lty = 1:length(shape), type = "l",
                log = logger, las = 1, cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, weibull = {
        # ymat <- matrix(0, nrow = length(x), ncol = length(shape))
        # for (i in 1:length(shape)) {
        #   ymat[, i] <- (shape[i]/scale[i]) * (x/scale[i])^(shape[i] - 1)
        # }
        matplot(x, 
                y = dist.outer(x, shape, dummy = scale, dweibull)/(dist.outer(x, shape, dummy = scale, pweibull)), 
                lty = 1:length(shape), type = "l",
                log = logger, las = 1, cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, normal = {
        matplot(x, 
                y = dist.outer(x, shape, dummy = location,dnorm)/(dist.outer(-x, shape, dummy = -location,pnorm)), 
                lty = 1:length(shape), type = "l", log = logger,
                las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", 
                lwd = lwd, ...)
      }, sev = {
        matplot(x, 
                y = dist.outer(x, shape, dummy = location,dsev)/(dist.outer(x, shape, dummy = location,ssev)), 
                lty = 1:length(shape), type = "l", log = logger,
                las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", 
                lwd = lwd, ...)
      }, lev = {
        matplot(x, 
                y = dist.outer(x, shape, dummy = location,dlev)/(dist.outer(x, shape, dummy = location,slev)), 
                lty = 1:length(shape), type = "l", log = logger,
                las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", 
                lwd = lwd, ...)
      }, logistic = {
        matplot(x, 
                y = dist.outer(x, shape, dummy = location,dlogis)/(dist.outer(-x, shape, dummy = -location,plogis)), 
                lty = 1:length(shape), type = "l",
                log = logger, las = 1, cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, loglogistic = {
        matplot(x, 
                y = dist.outer(x, shape, 0, dloglogis)/(dist.outer(1/(x), shape, 0, ploglogis)), 
                lty = 1:length(shape),
                type = "l", log = logger, las = 1, cex = 1.2, cex.axis = 1.1,
                xlab = "", ylab = "", lwd = lwd, ...)
      }, gamma = {
        matplot(x, 
                y = outer(x, shape, dgamma)/(outer(1/x, shape, pgamma)), 
                lty = 1:length(shape), type = "l",
                log = logger, las = 1, cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, igau = {
        matplot(x, 
                y = outer(x, shape, digau)/(outer(x, shape, sigau)), 
                lty = 1:length(shape), type = "l", log = logger,
                las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", 
                lwd = lwd, ...)
      }, bisa = {
        matplot(x, 
                y = outer(x, shape, dbisa)/(outer(x, shape,sbisa)), 
                lty = 1:length(shape), type = "l", log = logger,
                las = 1, cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", 
                lwd = lwd, ...)
      }, goma = {
        matplot(x, 
                y = dist.outer(x, shape, shape2, dgoma)/(dist.outer(x,shape, shape2, sgoma)), 
                lty = 1:length(shape),
                type = "l", log = logger, las = 1, cex = 1.2, cex.axis = 1.1,
                xlab = "", ylab = "", lwd = lwd, ...)
      }, lognormal = {
        matplot(x, 
                y = dist.outer(x, shape, 0, dlnorm)/(dist.outer(1/(x),shape, 0, plnorm)), 
                lty = 1:length(shape), type = "l",
                log = logger, xlab = "", ylab = "", lwd = lwd, las = 1,
                cex = 1.2, cex.axis = 1.1, ...)
      })

      title(xlab = "t", cex.lab = 1.5)
      usr.par <- par("usr")
      mtext(side = 2, line = 3, srt = 90, "h(t)", cex = 1.1)
      if (is.null(my.subtitle))
        title("Hazard Function", cex = 1)
    }
    sf.plot <- function(x, distribution, shape, scale, location,
                        shape2, ...) {
      par(mar = c(4.25, 5, 3.25, 2) + 0.1)
      switch(generic.distribution(distribution), exponential = {
        matplot(1/x, y = outer(x, 1/shape, pexp), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, weibull = {
        matplot(1/x, y = outer(x, shape, pweibull), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, loglogistic = {
        matplot(1/x, y = outer(x, shape, ploglogis), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, gamma = {
        matplot(1/x, y = outer(x, shape, pgamma), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, igau = {
        matplot(x, y = outer(x, shape, sigau), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, bisa = {
        matplot(x, y = outer(x, shape, sbisa), lty = 1:length(shape),
                type = "l", yaxt = "n", cex = 1.2, cex.axis = 1.1, xlab = "",
                ylab = "", lwd = lwd, ...)
      }, goma = {
        matplot(x, y = dist.outer(x, shape, shape2, sgoma),
                lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, sev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  ssev), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, lev = {
        matplot(x, y = dist.outer(x, shape, dummy = location,
                                  slev), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      }, logistic = {
        matplot(x, y = dist.outer(-x, shape, dummy = location,
                                  plogis), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, ...)
      }, normal = {
        matplot(x, y = dist.outer(-x, shape, dummy = location,
                                  pnorm), lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, ...)
      }, lognormal = {
        matplot(x, y = dist.outer(1/(x), shape, 0, plnorm),
                lty = 1:length(shape), type = "l", yaxt = "n",
                cex = 1.2, cex.axis = 1.1, xlab = "", ylab = "", lwd = lwd, ...)
      })
      cdf.ylabs <- c("0", ".5", "1")
      axis(2, at = as.numeric(cdf.ylabs), labels = cdf.ylabs,
           cex = 1.2, las = 1, hadj = .4)
      usr.par <- par("usr")
      xpos <- usr.par[1] - 0.18 * (usr.par[2] - usr.par[1])
      ypos <- usr.par[3] + 0.5 * (usr.par[4] - usr.par[3])
      mtext(side = 2, line = 3, srt = 90, "S(t)", cex = 1.1)
      title(xlab = "t", cex.lab = 1.5)
      if (is.null(my.subtitle))
        title("Survival Function", cex = 1)
    }
    parameter.plot <- function(distribution, shape, scale, location,
                               shape2, cexchar = 2, char.fudge = "", exponential2 = T) {

      cexchar <- cexchar * 0.5
      par(mar = c(4.25, 5, 3.25, 2) + 0.1)
      par(xaxt = "n", yaxt = "n", bty = "n")
      parameter2.name <- NULL
      parameter1 <- shape

        char.eta <-   "eta  "
        char.mu <-    "mu   "
        char.nu <-    "nu   "
        char.sigma <- "sigma"
        char.kappa <- "kappa"
        char.theta <- "theta"
        char.zeta <-  "zeta "
        char.gamma <- "gamma"
        char.eta <-   "eta  "
        char.beta <-  "beta "
        char.xi <-    "xi   "

      switch(generic.distribution(distribution), exponential = {
        parameter1.name <- char.theta
        if (exponential2) {
          textxy1 <- c(0.1, 0.92)
          parameter2.name <- char.gamma
        } else {
          textxy1 <- c(0.3, 0.9)
          parameter2.name <- NULL
        }
        parameter2 <- rep(0, length(parameter1))
      }, normal = {
        parameter2 <- location
        parameter2.name <- char.mu
        parameter1.name <- char.sigma
      }, sev = {
        parameter2 <- location
        parameter2.name <- char.mu
        parameter1.name <- char.sigma
      }, lev = {
        parameter2 <- location
        parameter2.name <- char.mu
        parameter1.name <- char.sigma
      }, logistic = {
        parameter2 <- location
        parameter2.name <- char.mu
        parameter1.name <- char.sigma
      }, loglogistic = {
        parameter2 <- location
        parameter1.name <- char.sigma
        parameter2.name <- char.mu
      }, gamma = {
        parameter2 <- scale
        parameter1.name <- char.kappa
        parameter2.name <- char.theta
      }, igau = {
        parameter2 <- scale
        parameter1.name <- char.beta
        parameter2.name <- char.theta
      }, bisa = {
        parameter2 <- scale
        parameter1.name <- char.beta
        parameter2.name <- char.theta
      }, goma = {
        parameter2 <- shape2
        parameter3 <- scale
        parameter1.name <- char.zeta
        parameter2.name <- char.eta
        parameter3.name <- char.theta
      }, weibull = {
        parameter2 <- scale
        parameter1.name <- char.beta
        parameter2.name <- char.eta
      }, lognormal = {
        parameter2 <- 0
        parameter1.name <- char.sigma
        parameter2.name <- char.mu
      }, )
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", lwd = lwd,
           cex = 1.2, cex.axis = 1.1, frame.plot = FALSE)
      if (is.null(parameter2.name)) {

        legend("top",
               parse(text = paste(parameter1.name,"~~",format(parameter1, nsmall = 2), sep = "~")),
               col = c(0,1:length(parameter1)), lty = c(0,1:length(parameter1)),
               cex = 1.25, lwd = 2, y.intersp = 1.25-0.05*length(parameter1),
               bty = "n", inset = -0.05)
      } else {

        legend("top",
               c(parse(text = paste(parameter1.name,"~~~~~~~~~~", parameter2.name,sep = "~")),
                 paste(format(parameter1, nsmall = 2),format(parameter2, nsmall = 2), sep = "      ")),
               col = c(0,1:length(parameter1)), lty = c(0,1:length(parameter1)),
               cex = 1.5-0.05*length(parameter1), y.intersp = 1.25-0.05*length(parameter1), lwd = 2,
               bty = "n", inset = -0.05)
      }
    }
    if (axsi)
      old.par <- par(yaxs = "i", xaxs = "i")
    else old.par <- par()
    if (original.par)
      on.exit({
        old.par[c("cin", "cra", "csi", "cxy", "din", "page")] <- NULL
        par(old.par)
        par(mfrow = c(1,1))
        par(new = F)
      })
    xx <- distribution.plot.range(distribution = distribution,
                                  shape = shape, scale = scale, prob.range = prob.range,
                                  location = location, shape2 = shape2)
    x <- seq(xx[1], xx[2], length = number.points)
    switch(type, all = {
      if (!is.null(my.title) && my.title == "") {
        par(mfrow = c(2, 2), oma = c(0, 0, 0, 0))
      } else {
        par(mfrow = c(2, 2), oma = c(0, 0, 0, 0))
      }
      cdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
      pdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
      if (length(shape) > 1) {
        hf.plot(x, distribution, shape, scale, plot.haz.log,
                location, shape2, ...)
        parameter.plot(distribution, shape, scale, location,
                       shape2, char.fudge = char.fudge, exponential2 = exponential2)
      } else {
        sf.plot(x, distribution, shape, scale, location,
                shape2, ...)
        hf.plot(x, distribution, shape, scale, plot.haz.log,
                location, shape2, ...)
      }
      if (is.null(my.title)) mtext(side = 3, line = 0, cex = 1.5,
                                   outer = TRUE, paste(distribution, " Distribution"))
    }, pdfcdf = {
      par(mfrow = c(1, 2), oma = c(0, 0, 0, 0))
      cdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
      pdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
      if (is.null(my.title)) mtext(side = 3, line = 0, cex = 1.5,
                                   outer = TRUE, paste(distribution, " Distribution"))
    }, pdf = {
      par(oma = c(0, 0, 0, 0))
      pdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
    }, cdf = {
      par(oma = c(0, 0, 0, 0))
      cdf.plot(x, distribution, shape, scale, location, shape2,
               ...)
    }, sf = {
      par(oma = c(0, 0, 0, 0))
      sf.plot(x, distribution, shape, scale, location, shape2,
              ...)
    }, hf = {
      par(oma = c(0, 0, 0, 0))
      hf.plot(x, distribution, shape, scale, plot.haz.log,
              location, shape2, ...)
    })
    if (type != "all") {
      if (is.null(my.title))
        mtext(side = 3, line = 0, cex = 1.5, outer = TRUE,
              paste(distribution, " Distribution"))
    }
  }

