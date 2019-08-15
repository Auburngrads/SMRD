#' Title
#'
#' @param data.ddd 
#' @param distribution 
#' @param transformation.response 
#' @param transformation.time 
#' @param ylim 
#' @param xlim 
#' @param my.title 
#' @param ylab 
#' @param xlab 
#' @param cex 
#' @param cex.labs 
#' @param cex.points 
#' @param add 
#' @param grids 
#' @param title.option 
#' @param pch.point 
#' @param response.on.yaxis 
#' @param subset 
#' @param do.legend 
#' @param fail.level 
#' @param group.var 
#' @param plot.lines 
#' @param lty 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' InsulationBrkdwn.ddd <- frame.to.ddd(insulationbrkdwn,
#'                                      response.column = 3, 
#'                                      time.column = 1,
#'                                      x.columns = 2,
#'                                      data.title = "Voltage Breakdown Data",
#'                                      response.units = "Volts",
#'                                      time.units = "Weeks")
#' 
#' print(InsulationBrkdwn.ddd)
#' 
#' plot(InsulationBrkdwn.ddd,
#'      transformation.Response = "log",
#'      transformation.time = "linear")
#' 
#' tmp <- groupi.Dest.Degrad.indivplots(InsulationBrkdwn.ddd,
#'                                      transformation.Response = "log", 
#'                                      transformation.time = "linear",
#'                                      distribution = "normal")
#' 
#' groupi.Dest.Degrad.oneplot(InsulationBrkdwn.ddd,
#'                            transformation.Response = "log", 
#'                            transformation.time = "linear",
#'                            distribution="normal")
#'                            
#' groupm.Dest.Degrad(InsulationBrkdwn.ddd, 
#'                    distribution = "normal",
#'                    transformation.Response = "log10",
#'                    transformation.x = "invtemp",
#'                    transformation.time = "linear")
#' 
#' groupm.Dest.Degrad(InsulationBrkdwn.ddd, 
#'                    distribution = "normal",
#'                    transformation.Response = "log",
#'                    transformation.x = "arrhenius",
#'                    transformation.time="linear")
#' 
#' # Do individual analyses at each level of temperature
#' 
#' InsulationBrkdwn.groupi.Dest.Degrad <-groupi.Dest.Degrad(InsulationBrkdwn.ddd,
#'                                                          distribution = "normal",
#'                                                          transformation.Response = "log", 
#'                                                          transformation.time = "sqrt")
#' 
#' plot(InsulationBrkdwn.groupi.Dest.Degrad,
#'      transformation.x = "Arrhenius")
#' 
#' InsulationBrkdwn.groupm.Dest.Degrad <-groupm.Dest.Degrad(InsulationBrkdwn.ddd,
#'                                                          distribution = "normal", 
#'                                                          transformation.Response = "log",
#'                                                          transformation.x = "arrhenius", 
#'                                                          transformation.time = "sqrt")
#' 
#' InsulationBrkdwn.groupm.Dest.Degrad<-groupm.Dest.Degrad(InsulationBrkdwn.ddd,
#'                                                         distribution = "normal",
#'                                                         transformation.Response = "log",
#'                                                         transformation.x = "arrhenius",
#'                                                         transformation.time = "sqrt",
#'                                                         new.data = c("150,260"))
#' 
#' }
groupi.Dest.Degrad.oneplot <-
function (data.ddd, 
          distribution, 
          transformation.response, 
          transformation.time,
          ylim = c(NA, NA), 
          xlim = c(NA, NA), 
          my.title = NULL,
          ylab = NULL, 
          xlab = NULL, 
          cex = 1.05, 
          cex.labs = 1.05, 
          cex.points = 1,
          add = F, 
          grids = F, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          pch.point = NULL,
          response.on.yaxis = T, 
          subset = T, 
          do.legend = "On plot",
          fail.level = NULL, 
          group.var = 1:ncol(xmat(data.ddd)), 
          plot.lines = T,
          lty = NULL,
          lwd = 2,
          bty = `if`(grids, "o","L"),...)
{
  
`do.list<-` <- function (data.ld, value) { 
  
  attr(data.ld, "do.list") <- value
  return(data.ld)   
  
}

CheckString <- function (pat, str) { 
  
  return(regexpr(pat, str) > 0) 
  
}

    on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1))
    
    tran.data.ddd <- plot.Dest.Degrad.data(x = data.ddd,
                                           transformation.response = transformation.response, 
                                           transformation.time = transformation.time,
                                           ylim = ylim, 
                                           xlim = xlim, 
                                           my.title = "",
                                           ylab = ylab, 
                                           xlab = xlab, 
                                           cex = cex, 
                                           cex.labs = cex.labs,
                                           cex.points = cex.points, 
                                           grids = grids, 
                                           title.option = title.option,
                                           pch.point = pch.point, 
                                           response.on.yaxis = response.on.yaxis,
                                           subset = subset, 
                                           group.var = group.var, 
                                           do.legend = "Suppress",
                                           mar = c(4.5, 5.25, 3.5, 12.1),
                                           bty = bty)
    
    do.list <- do.list(tran.data.ddd)
    
    if (is.null(lty)) {
      
        `if`(GetSMRDDefault("SMRD.solid.lines"),
             lty <- rep(1, length(do.list)),
             lty <- (1:(length(do.list) + 1))[-2])
      
    }
    
    if (is.null(pch.point)) pch.point <- (1:(length(do.list) + 4))[-c(2, 6, 17, 19)]
    if (length(pch.point) == 1) pch.point <- rep(pch.point, length(do.list))
    
    two.stage.out <- two.stage.dest.degrad(tran.data.ddd, 
                                           distribution = distribution,
                                           double.count.zeros = F)
    
    ok.values <- attr(two.stage.out, "ok.values")
    slope <- attr(two.stage.out, "slope")
    intercept <- attr(two.stage.out, "intercept")
    the.slope.computed.list <- slope.computed.list(two.stage.out)
    the.done.list <- done.list(two.stage.out)
    plot.index <- match(the.slope.computed.list, do.list)
    
    for (i in 1:length(the.slope.computed.list)) {
      
        if (map.SMRDDebugLevel() >= 4) {
          
            cat(the.slope.computed.list[i], 
                "Intercept = ", 
                intercept[i], 
                "slope = ", 
                slope[i], 
                "in groupi.Dest.Degrad.oneplot\n")
          
        }
      
        abline(intercept[i], 
               slope[i], 
               col = plot.index[i], 
               lty = lty[plot.index[i]],
               lwd = lwd)
        
        if (!is.null(fail.level)) abline(h = f.relationship(fail.level, transformation.response),
                                         lwd = 3, 
                                         lty = lty[i])
        
    }
    
    model.string <- paste("Resp:",
                          transformation.response, 
                          ",Time:",
                          transformation.time, 
                          ", Dist:", 
                          distribution, sep = "")
    
    if (is.null(my.title)) {
      
        my.title <- paste(get.data.title(tran.data.ddd), 
                          "\nDestructive Degradation",
                          " Individual Regression Analyses\n", 
                          model.string,
                          sep = "")
        
    }

    if (CheckString("full", title.option)) mtext(text = my.title, 
                                                 side = 3, 
                                                 cex = 1.2, 
                                                 line = 0.5)
    
    `if`(mean(slope) > 0,
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.99) },
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.99) })

        if (length(do.list) > 1) {
          
            if (do.legend == "On plot") {
              
                par(xpd = T)
              
                legend(legend.x, 
                       legend.y, 
                       parse(text = switch.units(do.list, NULL)), 
                       cex = cex,
                       bty = "n", 
                       col = 1:length(do.list), 
                       pch = pch.point,
                       lty = lty, 
                       y.intersp = 1,
                       seg.len = 1.5,
                       lwd = 1.5,
                       adj = c(-0.1))
                
            }
          
            if (do.legend == "New page" || do.legend == "New file") {
              
                if (do.legend == "New file") postscript(file = "Save_legend.ps", 
                                                        horizontal = T)
              
                plot(c(0, 0), 
                     c(1, 1), 
                     xlab = "", 
                     ylab = "",
                     type = "n", 
                     xaxt = "n", 
                     yaxt = "n")
                
                legend(x.loc(0.003), 
                       y.loc(0.997), 
                       do.list, 
                       cex = 1.1,
                       bty = "n", 
                       col = 1:length(do.list), 
                       pch = pch.point,
                       lty = lty, 
                       y.intersp = 0.675)
                
                if (do.legend == "New file") dev.off()
                
            }
          
        }

    attr(two.stage.out, "data.ddd") <- data.ddd
    attr(two.stage.out, "distribution") <- distribution
    attr(two.stage.out, "transformation Response") <- transformation.response
    attr(two.stage.out, "transformation.time") <- transformation.time
    oldClass(two.stage.out) <- "groupi.Dest.Degrad.out"
    invisible(two.stage.out)
    
}
