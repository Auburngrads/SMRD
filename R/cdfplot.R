cdfplot <-
function (data.ld,
          logx = F,
          my.title = NULL,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          xlab = get.time.units(data.ld),
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          band.type = "Pointwise",
          how.show.fhat = "step.fun",
          how.show.interval = "step.fun",
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          grids = 0,
          plot.censored.ticks = F,
          point.pch = 18,
          axsi = F,
          oldpar = T, ...)
{
  CheckString <- function (pat, str) { return(regexpr(pat, str) > 0) }

    if (axsi) {
        par(yaxs = "i", xaxs = "i")
        if (oldpar) on.exit(par(yaxs = "r", xaxs = "r"))
    }
  
    `if`(title.option == "blank",
         the.blank <- "blank",
         the.blank <- "blank2")
    
    `if`(logx,
         the.distribution <- "loguniform",
         the.distribution <- "Uniform")
    
    cdfest.out <- npprobplot(data.ld, 
                             distribution = the.distribution,
                             xlab = xlab, 
                             ylab = ylab, 
                             xlim = xlim, 
                             ylim = ylim,
                             band.type = band.type, 
                             conf.level = conf.level, 
                             title.option = the.blank,
                             my.title = "", 
                             plot.censored.ticks = plot.censored.ticks,
                             how.show.fhat = how.show.fhat, 
                             how.show.interval = how.show.interval,
                             point.pch = point.pch, 
                             grids = grids, 
                             trunc.correct = F,...)
    
    default.title <- get.data.title(cdfest.out$data.ld)
    
    if (is.null(band.type) || band.type == "") band.type <- "none"
    
    if (is.character(band.type)) {
      
        if (is.null(my.title))
            switch(casefold(cdfest.out$band.type), pointwise = ,
                simultaneous = {
                  my.title <- paste(default.title, "\n", "Nonparametric CDF Estimate\nwith Nonparametric ",
                    cdfest.out$band.type, percent.conf.level(conf.level),
                    "Confidence Bands")
                }, {
                  my.title <- paste(default.title, "\n", "Nonparametric CDF Estimate")
                })
    }

    if (CheckString("full", title.option)) mtext(side = 3, 
                                                 line = 1, 
                                                 outer = F,
                                                 text = my.title,
                                                 cex = 1)
    
    invisible(cdfest.out)
    
}
