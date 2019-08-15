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
#' @param cex.tic.lab 
#' @param add 
#' @param grids 
#' @param title.option 
#' @param pch.point 
#' @param response.on.yaxis 
#' @param group.var 
#' @param subset 
#' @param fail.level 
#' @param plot.lines 
#' @param do.legend 
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
groupi.Dest.Degrad.indivplots <-
function (data.ddd, 
          distribution,
          transformation.response = "log",
          transformation.time = "linear",
          ylim = c(NA, NA),
          xlim = c(NA, NA),
          my.title = NULL,
          ylab = NULL,
          xlab = NULL,
          cex = 0.5,
          cex.labs = 1,
          cex.points = 1,
          cex.tic.lab = 1,
          add = F,
          grids = F,
          title.option = NULL,
          pch.point = NULL,
          response.on.yaxis = T,
          group.var = 1:ncol(xmat(data.ddd)),
          subset = T,
          fail.level = NULL,
          plot.lines = T,
          do.legend = "On plot")
{
    `do.list<-` <- function (data.ld, value) { 
      
      attr(data.ld, "do.list") <- value
      return(data.ld)  
      
      }
    .do.list <- function (data.d) { 
      
      return(attr(data.d, "do.list")) 
      
    }
    
    func.call <- match.call()
    subset <- get.subset.vector(subset, data.ddd)
    subset.name <- attr(subset, "subset.name")
    data.ddd <- data.ddd[subset, ]
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
                                     SMRD.NameOnPlot = "")
    
    model.string <- paste("Resp:", transformation.response, ",Time:",
                          transformation.time, 
                          paste(", Dist:", distribution, sep = ""),
                          sep = "")
    
    if (is.null(my.title)) my.title <- paste(get.data.title(data.ddd), 
                                             subset.name, 
                                             "\n", model.string)
    
    if (length(group.var) == 1 && is.na(group.var)) {
      
        the.xmat <- as.data.frame(matrix(rep(1, length = nrow(Response(data.ddd)))))
        group.var <- 1
        do.list(data.ddd) <- 1
        
  } else {
    
        the.xmat <- as.matrix(xmat(data.ddd)[, group.var, drop = F])
        xmat(data.ddd) <- the.xmat
        do.list(data.ddd) <- get.x.markers(data.ddd, 
                                           group.var = group.var,
                                           long = T, 
                                           include.complete = T)
        
        complete.list(data.ddd) <- complete.list(do.list(data.ddd))
        
  }
    
    hold.warn <- options(warn = 0)
    tmp.orig.param <- two.stage.dest.degrad(data.ddd, 
                                            distribution = distribution,
                                            double.count.zeros = F)
    
    options(hold.warn)
    slope <- attr(tmp.orig.param, "slope")
    the.done.list <- done.list(tmp.orig.param)
    the.times <- times(data.ddd)
    
    if (is.null(ylab)) ylab <- get.response.units(data.ddd)
    if (is.null(xlab)) xlab <- get.time.units(data.ddd)
    transformation.time.name <- transformation.time
    
    if (generic.relationship.name(transformation.time) == "Arrhenius") {
      
        transformation.time <- "Arrhenius3"
        transformation.time <- set.relationship.power(transformation.time,
                                                      power)
    }
    transformation.response.in <- transformation.response
    
    if (generic.relationship.name(transformation.response) == "Arrhenius") {
      
        transformation.response <- "Arrhenius3"
        transformation.response <- set.relationship.power(transformation.response,
                                                          power)
        
    }
    
    if (!is.null(title.option) && 
         length(title.option) > 0 && 
        title.option == "blank") my.title <- ""
    
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(Response(data.ddd))[yrna]
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(the.times)[xrna]
    
   Response(data.ddd) <- as.matrix(f.relationship(Response(data.ddd),
                                                  transformation.response))
   
    times(data.ddd) <- f.relationship(times(data.ddd), transformation.time)
    the.times <- times(data.ddd)
    theResponse <-Response(data.ddd)
    two.stage.out <- two.stage.dest.degrad(data.ddd,
                                           distribution = distribution,
                                           double.count.zeros = F)
    
    slope.trans <- attr(two.stage.out, "slope")
    intercept.trans <- attr(two.stage.out, "intercept")
    slope.computed.list <- slope.computed.list(two.stage.out)
    the.done.list <- done.list(two.stage.out)
    
    for (i in 1:length(slope)) {
      
        if (map.SMRDDebugLevel() >= 4) cat(slope.computed.list[i], 
                                           "Intercept=", 
                                           intercept.trans[i],
                                           "slope=", 
                                           slope.trans[i], 
                                           "in groupi.Dest.Degrad.indivplots\n")
      
    }
    
    the.do.list <- do.list(data.ddd)
    old.par <- par(mfrow = get.mfcol.vec(length(the.do.list)),
                   oma = c(0, 2, 2, 0), 
                   err = -1)
    
    on.exit({
        par(old.par)
        par(new = F)
    })
    
    the.censor.codes <- censor.codes(data.ddd)
    the.case.weights <- case.weights(data.ddd)
    dummy.obs <- the.censor.codes == 0 | the.case.weights == 0
    
    if (is.null(pch.point)) pch.point <- (1:(length(the.do.list) + 4))[-c(2, 6, 17, 19)]
    if (length(pch.point) == 1) pch.point <- rep(pch.point, length(the.do.list))
    
    plot.index <- match(slope.computed.list, the.do.list)
    slope.ok <- !is.na(match(the.do.list, slope.computed.list))
    
    for (i in 1:length(the.do.list)) {
      
        the.color <- plot.index[i]
        
        if (!add) plot.paper(x = xlim, 
                             y = ylim,
                             x.axis = transformation.time,
                             y.axis = transformation.response,
                             ylab = "",
                             xlab = "",
                             response.on.yaxis = response.on.yaxis,
                             cex = cex,
                             cex.labs = cex.labs,
                             cex.tic.lab = cex.tic.lab,
                             grids = grids)
        
        if (is.character(the.do.list[i])) {
          
            mtext(text = parse(text = switch.units(the.do.list[i])),
                  side = 3, 
                  cex = 1,
                  line = 1)
          
        }
        
        the.ones <- the.do.list[i] == complete.list(data.ddd)
        rcensored <- the.ones & the.censor.codes == 2 & !dummy.obs
        ncensored <- the.ones & the.censor.codes == 1 & !dummy.obs
        lcensored <- the.ones & the.censor.codes == 3 & !dummy.obs
        icensored <- the.ones & the.censor.codes == 4 & !dummy.obs
        
        if (response.on.yaxis) {
          
            if (any(ncensored))
                points.default(as.vector(the.times[ncensored]),
                               theResponse[ncensored, 1], 
                               pch = pch.point[i],
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = the.color)
          
            if (any(rcensored))
                points.default(as.vector(the.times[rcensored]),
                               theResponse[rcensored, 1], 
                               pch = 17, 
                               cex = (1.2 * cex.points * GetSMRDDefault("SMRD.point.size"))/100, 
                               col = the.color)
          
            if (any(lcensored))
                points.default(as.vector(the.times[lcensored]),
                               theResponse[lcensored, 1], 
                               pch = 19, 
                               cex = (1.2 * cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = the.color)
          
            if (any(icensored))
                points.default(as.vector(the.times[icensored]),
                               (theResponse[icensored, 1] + theResponse[icensored, 2])/2, 
                               pch = 4, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = the.color)
          
      } else {
        
            if (any(ncensored))
                points.default(theResponse[ncensored, 1], 
                               as.vector(the.times[ncensored]),
                               pch = pch.point[i], 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100,
                               col = the.color)
        
            if (any(rcensored)) {
              
                rh.eps <- 0.001
                rh.size <- 0.1
                arrows(theResponse[rcensored, 1] - rh.eps, 
                       as.vector(the.times[rcensored]),
                       theResponse[rcensored, 1] + rh.eps, 
                       as.vector(the.times[rcensored]),
                       size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100,
                       open = F, 
                       rel = F, 
                       col = the.color)
                
            }
        
            if (any(lcensored)) {
              
                rh.eps <- 0.001
                rh.size <- 0.1
                arrows(theResponse[lcensored, 1] + rh.eps, 
                       as.vector(the.times[lcensored]),
                       theResponse[lcensored, 1] - rh.eps, 
                       as.vector(the.times[lcensored]),
                       size = (rh.size * cex * GetSMRDDefault("SMRD.point.size"))/100, 
                       open = F,
                       rel = F,
                       col = the.color)
                
            }
        
            if (any(icensored))
                points.default((theResponse[icensored, 1] + theResponse[icensored, 2])/2, 
                               as.vector(the.times[icensored]), 
                               pch = 4, 
                               cex = (cex.points * GetSMRDDefault("SMRD.point.size"))/100, 
                               col = the.color)
        
      }
        
        if (plot.lines && slope.ok[i]) {
          
            abline(intercept.trans[i], slope.trans[i], col = the.color)
          
        }
        
        if (!is.null(fail.level)) abline(h = f.relationship(fail.level, transformation.response),
                                         lwd = 3)
        
    }
    
    #mtext(text = my.title, side = 3, cex = 1.2, line = -2, outer = T)
    mtext(text = xlab, side = 1, cex = 1.2, line = -2, outer = T)
    mtext(text = ylab, side = 2, cex = 1.2, line = -2, outer = T)
    SMRDOptions(save.SMRD.options)
    attr(two.stage.out, "data.ddd") <- data.ddd
    attr(two.stage.out, "distribution") <- distribution
    attr(two.stage.out, "transformation Response") <- transformation.response
    attr(two.stage.out, "transformation.time") <- transformation.time
    oldClass(two.stage.out) <- "groupi.Dest.Degrad.out"
    invisible(two.stage.out)
    
}
