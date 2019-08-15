#' Title
#'
#' @param data.ddd 
#' @param transformation.response 
#' @param transformation.time 
#' @param transformation.x 
#' @param distribution 
#' @param group.var 
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
#' @param do.legend 
#' @param subset 
#' @param pch.point 
#' @param response.on.yaxis 
#' @param new.data 
#' @param FailLevel 
#' @param power 
#' @param PlotFailDefLine 
#' @param debug1 
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
groupm.Dest.Degrad <-
function (data.ddd,
          transformation.response,
          transformation.time,
          transformation.x,
          distribution,
          group.var = 1:ncol(xmat(data.ddd)),
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
          do.legend = "On plot",
          subset = T,
          pch.point = NULL,
          response.on.yaxis = T,
          new.data = NULL,
          FailLevel = NULL,
          power = NULL,
          PlotFailDefLine = T,
          debug1 = map.SMRDDebugLevel(),
          lty = NULL,
          lwd = 2,
          mar = c(4.5, 5.25, 3.5, 12.1),
          bty = `if`(grids, "o", "L"),...)
{

  CheckString <- function (pat, str) { return(regexpr(pat, str) > 0) }

  ADDTZeroRepsFilter <-
    function (data.ddd, group.var = 1:ncol(the.xmat))
    {
      the.times <- times(data.ddd)
      zero.times <- the.times == 0
      
      if (!any(zero.times)) return(data.ddd)
      
      the.xmat <- xmat(data.ddd)
      the.markers <- apply(the.xmat[, group.var, drop = F], 1,
                           paste, sep = "", collapse = ";")
      
      the.unique.markers <- unique(the.markers)
      the.first.zeros <- the.markers == the.unique.markers[1] &
        zero.times
      
      if (!any(the.first.zeros)) return(data.ddd)
      
      first.response <-Response(data.ddd)[the.first.zeros]
      reps.found <- rep(F, 
                        length = length(the.unique.markers) - 1)
      
      for (i in 2:length(the.unique.markers)) {
        
        the.current.zeros <- the.markers == the.unique.markers[i] &
          zero.times
        current.response <- Response(data.ddd)[the.current.zeros]
        
        if (length(current.response) > 0 && 
            length(first.response)   > 0 && 
            (length(first.response) == length(current.response) && 
             all(sort(first.response) == sort(current.response))))
          
          reps.found[i - 1] <- T
        
      }
      
      if (F && !any(reps.found)) warning("There are 0 times, but no replication.\n  No filtering will be done.\n")
      
      if (any(reps.found) && !all(reps.found)) {
        
        warning(paste("Some reps at time 0 found in the data set.\nNo filtering will be done.\n"))
        
      }
      
      if (all(reps.found)) {
        
        cat("\nFiltering 0-time replicates for analysis.\n")
        the.filtered.zeros <- the.markers != the.unique.markers[1] &
          zero.times
        the.return <- data.ddd[!the.filtered.zeros, ]
        attr(the.return, "did.filter") <- T
        return(the.return)
        
      }
      
      attr(data.ddd, "did.filter") <- F
      return(data.ddd)
      
    }

    transformation.x <- set.relationship.power(transformation.x,
                                               power)
    
    data.ddd <- ADDTZeroRepsFilter(data.ddd, group.var = group.var)
    subset <- get.subset.vector(subset, data.ddd)
    subset.name <- attr(subset, "subset.name")
    data.ddd <- data.ddd[subset, ]
    the.do.list.orig <- get.x.markers(data.ddd, 
                                      group.var = group.var,
                                      long = T, 
                                      include.complete = T)
    
    the.complete.list.orig <- complete.list(the.do.list.orig)
    the.xmat <- xmat(data.ddd)[, group.var, drop = F]
    relationship.sanity(the.xmat, transformation.x)
    relationship.sanity(times(data.ddd), 
                        transformation.time,
                        "Transformation for Time")
    
    relationship.sanity(Response(data.ddd), 
                        transformation.response,
                        "Transformation for Response")
    
    x.names <- (dimnames(the.xmat)[[2]])

    on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1))
    
    trans.data.ddd <- plot.Dest.Degrad.data(x = data.ddd,
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
                                            do.legend = "suppress", 
                                            group.var = group.var,
                                            mar = mar,
                                            bty = bty)
    
    gmle.out <- dest.degrad.mle(data.ddd, 
                                group.var = group.var,
                                distribution = distribution, 
                                transformation.response = transformation.response,
                                transformation.time = transformation.time, 
                                transformation.x = transformation.x,
                                power = power,
                                debug1 = debug1)
    
    if (!is.null(new.data)) {
      
        if (map.SMRDDebugLevel() >= 4) {
            cat("in groupm.Dest.Degrad, new.data\n")
            print(new.data)
            
        }
        new.data <- as.matrix(frame.new.data(new.data, gmle.out))
        the.do.list.new.data <- get.x.markers(new.data, 
                                              sending.xmat = T,
                                              long = T, 
                                              include.complete = T)
        
        the.complete.list.new.data <- complete.list(the.do.list.new.data)
        the.do.list <- unique(c(the.do.list.orig, the.do.list.new.data))
        the.complete.list <- c(the.complete.list.orig, the.complete.list.new.data)
        the.xmat.plus <- rbind(the.xmat, new.data)
        
  } else {
    
        the.do.list <- the.do.list.orig
        the.xmat.plus <- the.xmat
        the.complete.list <- the.complete.list.orig
        
  }
    
    stresses.plus <- the.xmat.plus[1:length(the.do.list), , drop = F]
    
    for (i in 1:length(the.do.list)) {
      
        sub.xmat <- the.xmat.plus[the.do.list[i] == the.complete.list,
            , drop = F]
        stresses.plus[i, ] <- sub.xmat[1, , drop = F]
        
    }
    
    if (is.null(lty)) {
      
        `if`(GetSMRDDefault("SMRD.solid.lines"),
             lty <- rep(1, length(the.do.list)),
             lty <- (1:(length(the.do.list) + 1))[-2])
      
    }
    
    leave.symbol.out <- c(2, 6, 17, 19)
    if (is.null(pch.point)) pch.point <- (1:(length(the.do.list) + length(leave.symbol.out)))[-leave.symbol.out]
    
    sub.model <- get.sub.model.dest.degrad.mle.out(gmle.out,
                                                   stresses = stresses.plus)
    
    gmle.out$residuals <- get.residuals(gmle.out)
    
    x.columns(data.ddd) <- c(get.x.columns(data.ddd), 
                             get.time.column(data.ddd))
    
    gmle.out$data.ld <- data.ddd
    gmle.out$the.orig.data.ld <- data.ddd
    gmle.out$analysis.type <- "Destructive Degradation"
    x.names <- (dimnames(the.xmat)[[2]])
    gmle.out$relationship <- paste("Resp:", 
                                   gmle.out$model$transformation.response,
                                   ",Time:", 
                                   gmle.out$model$transformation.time, 
                                   ",", 
                                   paste(x.names, transformation.x, collapse = ",", sep = ":"), 
                                   sep = "")
    
    relationship.string <- gmle.out$relationship
    gmle.out$distribution <- gmle.out$model$distribution
    
    if (map.SMRDDebugLevel() >= 4) cat("number of char in relationships = ", 
                                       nchar(relationship.string),"\n")
    
    if (nchar(relationship.string) > 50) {
      
      relationship.string <- paste("Resp:", 
                                   gmle.out$model$transformation.response,
                                   ",Time:", 
                                   gmle.out$model$transformation.time, 
                                   ",",
                                   paste(abbreviate(x.names), 
                                         abbreviate(transformation.x),collapse = ",", sep = ":"), 
                                   sep = "")
      
    }
    
    gmle.out$relationship.string <- relationship.string
    slope <- as.vector(sub.model$slope)
    intercept <- sub.model$intercept
    cat("\nEstimates at each level of the explanatory variable(s):\n")
    
    for (i in 1:length(slope)) {
      
        cat("Model slope at", 
            the.do.list[i], 
            "Intercept = ", format(intercept[i]),
            "slope = ", format(slope[i]), "\n")
      
        abline(intercept[i], 
               slope[i], 
               col = i, 
               lty = lty[i],
               lwd = 2)
        
    }
    
    if (!is.null(FailLevel) && PlotFailDefLine) {
      
        abline(h = f.relationship(FailLevel, transformation.response),
               lwd = 3)
      
    }
    
    if (is.null(my.title)) {
      
        my.title <- paste(get.data.title(trans.data.ddd), "\n",
                          gmle.out$analysis.type, 
                          " Regression Analyses\n",
                          relationship.string, 
                          paste(", Dist:", gmle.out$model$distribution, sep = ""), 
                          sep = "")
        
    }

        title.line.adj <- 0.5

    if (CheckString("full", title.option)) mtext(text = my.title, 
                                                 side = 3, 
                                                 cex = 1.2, 
                                                 line = title.line.adj)
        
    `if`(mean(slope) > 0,
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.95)  },
         { legend.x <- x.loc(1.05) ; legend.y <- y.loc(0.95)  })
    
    the.pch.point <- -pch.point
    the.pch.point[1:length(the.do.list.orig)] <- -the.pch.point[1:length(the.do.list.orig)]

        if (do.legend == "On plot") {
          
            par(xpd = T)
          
            legend(legend.x, 
                   legend.y, 
                   parse(text = switch.units(the.do.list)), 
                   cex = cex,
                   bty = "n", 
                   col = 1:length(the.do.list), 
                   pch = the.pch.point[1:length(the.do.list)],
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
                   y.loc(0.994), 
                   the.do.list, 
                   cex = 1.1,
                   bty = "n", 
                   col = 1:length(the.do.list), 
                   pch = the.pch.point[1:length(the.do.list)],
                   lty = lty,
                   y.intersp = 0.675)
            
            if (do.legend == "New file") dev.off()
        }

    attr(gmle.out, "sub.model") <- sub.model
    oldClass(gmle.out) <- c("groupm.Dest.Degrad.out", oldClass(gmle.out))
    MysetOldClass(attr(gmle.out, "class"))
    invisible(gmle.out)
    
}
