#' @export
plot.repeated.measures.data <-
function (x, 
          x.axis = "linear", 
          y.axis = "linear", 
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          type = "dlb", 
          xlab = attr(x,'time.units'),
          ylab = attr(x, 'response.units'), 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          my.title = NULL,
          fail.level = NULL, 
          subset = T, 
          pch = 1, 
          cex = 1, 
          cex.point = 0.8,
          stresses = get.x.markers(x, group.var = group.var),
          group.var = 1:length(the.x.columns),
          use.color = FALSE, ...)
{
    unit.column <- attr(x, "unit.column")
    Unit.marker <- x[[unit.column]]
    response.column <- attr(x, "response.column")
    time.column <- attr(x, "time.column")
    time.units <- attr(x, "time.units")
    data.note <- attr(x, "data.note")
    response.units <- attr(x, "response.units")
    xlabel <- attr(x, "xlabel")
    response.units <- get.response.units(data.d = x)
    the.x.columns <- get.x.columns(data.d = x)
    
    number.cases <- length(Unit.marker)
    response <- Response(data.d = x)
    
    if (!any(class(x) == "repeated.measures.data")) {
        stop(paste(deparse(substitute(x)), 
                   "is\nnot a repeated measures data object"))
    }
    
    if (!is.data.frame(x)) {
        stop("First argument must be a degradation data object")
    }
    
    if (is.null(the.x.columns) || any(is.na(group.var))) {
      
        single.plot.repeated.measures.data(data.rmd = x, x.axis = x.axis,
            y.axis = y.axis, xlim = xlim, ylim = ylim,
            type = type, xlab = xlab, ylab = ylab, title.option = title.option,
            my.title = my.title, subset = subset, fail.level = fail.level,
            pch = pch, cex = cex, cex.point = cex.point, use.color = use.color,
            ...)
      
        invisible()
        
    } else {
    
    subset <- get.subset.vector(subset, x)
    subset.name <- attr(subset, "subset.name")
    x <- x[subset, ]
    Unit.marker <- Unit.marker[subset]
    
    old.par <- par(mfrow = get.mfcol.vec(length(stresses)), 
                   oma = c(0, 4, 4, 0), 
                   err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
                                     SMRD.NameOnPlot = "")
    
    stress.names <- get.x.markers(data.ld = x, 
                                  group.var = group.var,
                                  long = T)
    
    for (i in 1:length(stresses)) {
      
        the.subset.x <- 
          multiple.get.rmd.subset(data.rmd = x,
                                  stresses[i], 
                                  columns = group.var)
        
        single.plot.repeated.measures.data(data.rmd = the.subset.x,
            x.axis = x.axis, y.axis = y.axis, xlim = xlim,
            ylim = ylim, type = type, xlab = xlab, ylab = ylab,
            my.title = stress.names[i], fail.level = fail.level,
            pch = pch, cex = cex, cex.point = cex.point, ...)
    }
    
    SMRDOptions(save.SMRD.options)
    
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.d = x), 
                          subset.name,
                          "\nx axis:", 
                          x.axis, "  y axis:", 
                          y.axis)
    }

    mtext(text = my.title, 
          side = 3, 
          outer = T, 
          line = 0.5,
          cex = 1.2)
    
    invisible()
    }
}
