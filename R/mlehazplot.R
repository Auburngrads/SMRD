#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param gamthr 
#' @param xlab 
#' @param xlim 
#' @param ylim 
#' @param time.vec 
#' @param conf.level 
#' @param my.title 
#' @param cex 
#' @param sub.title 
#' @param yaxis.line 
#' @param x.axis 
#' @param y.axis 
#' @param ylab 
#' @param fits 
#' @param grids 
#' @param theta.start 
#' @param parameter.fixed 
#' @param title.option 
#' @param print.parameters 
#' @param band.type 
#' @param plotem 
#' @param param.loc 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1, 
#'                                 censor.column = 3,
#'                                 time.units = "Kilometers")
#'
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Weibull", 
#'            param.loc = "topleft")
#' 
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Frechet")
#' 
#' 
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Lognormal")
#' 
#' 
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Lognormal",
#'            xlim = c(5000,500000), 
#'            y.axis = "log")
#' 
#' 
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Weibull",
#'            time.vec = c(10000,20000,30000))
#' 
#' mlehazplot(ShockAbsorber.ld, 
#'            distribution = "Weibull",
#'            time.vec = c(10000,20000,30000),
#'            parameter.fixed = c(F,T),
#'            theta.start = c(9.,2))
#' }
mlehazplot <-
function (data.ld, 
          distribution, 
          gamthr = 0, 
          xlab = get.time.units(data.ld),
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          time.vec = NULL,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          my.title = NULL,
          cex = 1, 
          sub.title = NULL, 
          yaxis.line = 8, 
          x.axis = "linear",
          y.axis = "linear", 
          ylab = NULL, 
          fits = F, 
          grids = F, 
          theta.start = c(NA, NA), 
          parameter.fixed = NULL, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          print.parameters = T,
          band.type = "p", 
          plotem = T, 
          param.loc = "bottomright",...)
{
    if (!missing(gamthr) && is.null(sub.title)) {
        sub.title <- paste("shift gamma = ", format(gamthr))
  
        } else {
          
        if (is.null(sub.title))
            sub.title <- ""
    
        }
  
    mlest.out <- mlest(data.ld, 
                       distribution, 
                       gamthr = gamthr,
                       theta.start = theta.start, 
                       parameter.fixed = parameter.fixed,...)
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(Response(data.ld))[xrna]
    
    if (conf.level > 0.1) {
      
        conf.int.title <- paste("\nand Pointwise", percent.conf.level(conf.level),
            "Confidence Intervals")
        } else {
          
        conf.int.title <- ""
    
        }
    
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.ld), "\n", distribution,
            "Distribution Hazard Function ML Estimate", conf.int.title)
    }
    if (title.option != "full")  my.title <- ""
    
    plot.hazard(mlest.out, 
                conf.level = conf.level, 
                ylim = ylim,
                xlim = xlim, 
                x.axis = x.axis, 
                y.axis = y.axis,
                fits = fits, 
                yaxis.line = yaxis.line, 
                xlab = xlab, ylab = ylab,
                grids = grids, 
                band.type = band.type, 
                plotem = T)
    
    f.print.parameters(mlest.out, 
                       print.parameters, 
                       param.loc = param.loc)
    
    if (is.null(time.vec)) {
      
        `if`(is.logdist(mlest.out$distribution),
             time.vec <- as.numeric(logax(xlim)$ticlab),
             time.vec <- as.numeric(linax(xlim)$ticlab))
      
    }
    
    the.table <- plot.hazard(mlest.out, 
                             conf.level = conf.level,
                             ylim = ylim, 
                             xlim = xlim, 
                             time.vec = time.vec,
                             x.axis, 
                             y.axis, 
                             fits = fits, 
                             yaxis.line = yaxis.line,
                             xlab = xlab, 
                             ylab = ylab, 
                             plotem = F)

    mtext(side = 3, 
          line = 0.5, 
          outer = F, 
          text = my.title,
          cex = cex)
    
    rlist <- list(the.table = the.table, 
                  conf.level = conf.level,
                  mlest.out = mlest.out)
    
    oldClass(rlist) <- "mlehazplot.out"
    invisible(rlist)
}
