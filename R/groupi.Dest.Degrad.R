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
#' @param lty 
#' @param plot.lines 
#' @param separate.plots 
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
#' groupm.Dest.Degrad(InsulationBrkdwn.ddd, 
#'                    distribution = "normal",
#'                    transformation.Response = "log10",
#'                    transformation.x = "invtemp",
#'                    transformation.time = "linear")
#' 
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
#' 
#' }
groupi.Dest.Degrad <-
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
          lty = NULL,
          plot.lines = T,
          separate.plots = F,
          lwd = 2,...)
{
    relationship.sanity(times(data.ddd), 
                        transformation.time,
                        "Transformation for Time")
    
    relationship.sanity(Response(data.ddd), 
                        transformation.response,
                        "Transformation for Response")
    
    if (separate.plots) {
        
        results <- 
            groupi.Dest.Degrad.indivplots(data.ddd = data.ddd,
                                          distribution = distribution, 
                                          transformation.response = transformation.response,
                                          transformation.time = transformation.time,
                                          ylim = ylim,
                                          xlim = xlim,
                                          my.title = my.title,
                                          ylab = ylab,
                                          xlab = xlab,
                                          cex = cex,
                                          cex.labs = cex.labs,
                                          cex.points = cex.points,
                                          add = add,
                                          grids = grids,
                                          title.option = title.option,
                                          pch.point = pch.point,
                                          response.on.yaxis = response.on.yaxis,
                                          subset = subset,
                                          do.legend = do.legend,
                                          fail.level = fail.level,
                                          group.var = group.var,
                                          plot.lines = plot.lines)
        
} else {
    
        results <- 
            groupi.Dest.Degrad.oneplot(data.ddd = data.ddd,
                                       distribution = distribution,
                                       transformation.response = transformation.response,
                                       transformation.time = transformation.time, 
                                       ylim = ylim,
                                       xlim = xlim,
                                       my.title = my.title,
                                       ylab = ylab,
                                       xlab = xlab,
                                       cex = cex,
                                       cex.labs = cex.labs,
                                       cex.points = cex.points,
                                       add = add,
                                       grids = grids,
                                       title.option = title.option,
                                       pch.point = pch.point,
                                       response.on.yaxis = response.on.yaxis,
                                       subset = subset,
                                       do.legend = do.legend,
                                       fail.level = fail.level,
                                       group.var = group.var,
                                       lty = lty,
                                       plot.lines = plot.lines,
                                       lwd = lwd)
        
}
    
    invisible(results)
    
}