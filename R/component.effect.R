component.effect <- 
  function (parallel = TRUE, 
            dependent = FALSE, 
            mu = logb(10),
            xlim = c(0, 1),
            ylim = c(0, 1),
            sd = 1, 
            rho = c(0, 0.4, 0.7, 0.9, 1)) 
  {
  
  if (parallel) {
    
     `if`(!dependent,
           parallel.effect(xrange = xlim, 
                           yrange = ylim),
           parallel.dep.effect(xrange = xlim, 
                               yrange = ylim, 
                               mu = mu, 
                               sd = sd, 
                               rho = rho))
    
  } else {
    
     `if`(!dependent,
          series.effect(xrange = xlim, 
                        yrange = ylim),
          series.dep.effect(xrange = xlim, 
                            yrange = ylim, 
                            mu = mu, 
                            sd = sd, 
                            rho = rho))
  
    }
}

#'
#'

parallel.effect <-
  function (xrange = c(0.5, 1), 
            yrange = c(0.8, 1), 
            ncomp = c(1, 2, 3, 4, 5)) 
  {
    old.par <- par(err = -1)
    on.exit({
      par(old.par)
      par(new = F)
    })
    par(mar = c(4.25, 4.5, 1.5, 3.1), err = -1)
    
    parallelrel <- function(x, n) return(1 - (1 - x) ^ n)
    
    xvec <- seq(xrange[1], xrange[2], length = 1000)
    plot(xrange, 
         yrange, 
         type = "n", 
         xlab = "", 
         ylab = "", 
         cex.axis = 1.1, 
         las = 1)
    
    box(lwd = 1)
    title(xlab = "Individual Component Reliability", 
          cex.lab = 1.1)
    title(ylab = "Parallel System Reliability", 
          cex.lab = 1.1, 
          mgp = c(3.5, 1, 0))
    
    color <- rainbow(length(ncomp),v = .75)
    
    for (i in 1:length(ncomp)) {
      lines(xvec, 
            parallelrel(xvec, ncomp[i]), 
            lwd = 2, 
            lty = i + 1, 
            col = color[i])
    }
    #     text(0.862757, 0.820588, "s=1", cex = 1.5)
    #     text(0.500543, 0.93, "s=4", cex = 1.5)
    #     text(0.500543, 0.87, "s=3", cex = 1.5)
    #     text(0.615618, 0.829603, "s=2", cex = 1.5)
    #     text(0.498999, 0.98, "s=5", cex = 1.5)
    
    
    
    legend(xrange[2]+(xrange[2]-xrange[1])*.05, 
           yrange[2], 
           legend = as.character(ncomp), 
           title = expression(bold('Units')),
           pch = 15, 
           col = color, 
           bty = "n", 
           yjust = 1, 
           y.intersp = .75, 
           xpd = T)
  }

#'
#'

parallel.dep.effect <-
  function (xrange = c(0.5, 0.999), 
            yrange = c(0.8, 1), 
            mu = logb(10), 
            sd = 1, 
            rho = c(0, 0.4, 0.7, 0.9, 1)) 
  {
    old.par <- par(err = -1)
    on.exit({
      par(old.par)
      par(new = F)
    })
    par(mar = c(4.25, 4.5, 1.5, 3.1), err = -1)
    xvec <- seq(xrange[1], xrange[2], length = 100)
    plot(xrange, 
         yrange, 
         type = "n", 
         xlab = "", 
         ylab = "", 
         cex.axis = 1.1, 
         las = 1)
    
    box(lwd = 1)
    title(xlab = "Individual Component Reliability", 
          cex.lab = 1.1)
    title(ylab = "Two-Component Parallel-System Reliability", 
          cex.lab = 1.1, 
          mgp = c(3.5, 1, 0))
    
    color <- rainbow(length(rho),v = .75)
    
    for (i in 1:length(rho)) {
      
      time.mark <- qnorm(1 - xvec, mu, sd)
      paralleldeprel <- 1 - bvnsw(mu, 
                                  mu, 
                                  sd, 
                                  sd, 
                                  rho[i], 
                                  time.mark, 
                                  time.mark)
      lines(xvec, 
            paralleldeprel, 
            lwd = 2, 
            lty = i + 1, 
            col = color[i])
    }
    #     else text(0.615618, 0.8296, "rho=0", cex = 1.5)
    #     text(0.683, 0.8296, ".4", cex = 1.5)
    #     text(0.742, 0.8296, ".7", cex = 1.5)
    #     text(0.794, 0.8296, ".9", cex = 1.5)
    #     text(0.847, 0.8296, "1", cex = 1.5)
    
    legend(xrange[2] + (xrange[2] - xrange[1]) * .05,
           yrange[2], 
           legend = as.character(rho), 
           title = expression(bold(rho)),
           pch = 15, 
           col = color, 
           bty = "n", 
           yjust = 1, 
           y.intersp = .75, 
           xpd = T)
  }

#'
#'

series.effect <-
  function (xrange = c(0.98, 1), 
            yrange = c(0, 1), 
            ncomp = c(1, 10, 25, 50, 100, 200, 400)) 
  {
    old.par <- par(err = -1)
    on.exit({
      par(old.par)
      par(new = F)
    })
    
    par(mar = c(4.25, 4.5, 1.5, 3.1), err = -1)
    seriesrel <- function(x, n) return((x) ^ n) 
    
    xvec <- seq(xrange[1], 1, length = 1000)
    plot(xrange, 
         yrange, 
         type = "n", 
         xlab = "", 
         ylab = "", 
         cex.axis = 1.1, 
         las = 1)
    
    box(lwd = 1)
    title(xlab = "Individual Component Reliability", 
          cex.lab = 1.1)
    title(ylab = "Series System Reliability", 
          cex.lab = 1.1, 
          mgp = c(3.5, 1, 0))
    
    color <- rainbow(length(ncomp),v = .75)
    
    for (i in 1:length(ncomp)) {
      lines(xvec, 
            seriesrel(xvec, ncomp[i]), 
            lwd = 2, 
            lty = i + 1, 
            col = color[i])
    }
    #     text(0.981, 0.94213, "s=1", cex = 1.1)
    #     text(0.981, 0.770429, "s=10", cex = 1.1)
    #     text(0.981, 0.54, "s=25", cex = 1.1)
    #     text(0.980979, 0.336884, "s=50", cex = 1.1)
    #     text(0.983142, 0.141574, "s=100", cex = 1.1)
    #     text(0.990803, 0.111526, "s=200", cex = 1.1)
    #     text(0.994727, 0.0600159, "s=400", cex = 1.1)
    
    legend(xrange[2] + (xrange[2] - xrange[1]) * .05,
           yrange[2], 
           legend = as.character(ncomp), 
           title = expression(bold('Units')),
           pch = 15, 
           col = color, 
           bty = "n", 
           yjust = 1, 
           y.intersp = .75, 
           xpd = T)
  }

#'
#'

series.dep.effect <-
  function (xrange = c(0.8, 0.999), 
            yrange = c(0.8, 0.999), 
            mu = logb(10), 
            sd = 1, 
            rho = c(0, 0.4, 0.7, 0.9, 1)) 
  {
    old.par <- par(err = -1)
    on.exit({
      par(old.par)
      par(new = F)
    })
    par(mar = c(4.25, 4.5, 1.5, 3.1), err = -1, pty = "s")
    xvec <- seq(xrange[1], xrange[2], length = 1000)
    plot(xrange, 
         yrange, 
         type = "n", 
         xlab = "", 
         ylab = "", 
         cex.axis = 1.1, 
         las = 1)
    box(lwd = 1)
    
    title(xlab = "Individual Component Reliability", 
          cex.lab = 1.1)
    title(ylab = "Two-Component Series-System Reliability", 
          cex.lab = 1.1, 
          mgp = c(3.5, 1, 0))
    
    color <- rainbow(length(rho),v = .75)
    
    for (i in 1:length(rho)) {
      time.mark <- qnorm(1 - xvec, mu, sd)
      seriesdeprel <- bvn(mu, 
                          mu, 
                          sd, 
                          sd, 
                          rho[i], 
                          time.mark, 
                          time.mark)
      lines(xvec, 
            seriesdeprel, 
            lwd = 2, 
            lty = i + 1, 
            col = color[i])
    }
    
    #     else text(0.808852, 0.826038, "rho=1", cex = 1.5)
    #     text(0.877544, 0.801693, ".4", cex = 1.5)
    #     text(0.862802, 0.806391, ".7", cex = 1.5)
    #     text(0.841787, 0.811089, ".9", cex = 1.5)
    #     text(0.905773, 0.799984, "0", cex = 1.5)
    
    legend(xrange[2] + (xrange[2] - xrange[1]) * .05,yrange[2], 
           legend = as.character(rho), 
           title = expression(bold(rho)),
           pch = 15, 
           col = color, 
           bty = "n", 
           yjust = 1, 
           y.intersp = .75, 
           xpd = T)
  }
