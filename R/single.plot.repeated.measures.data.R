single.plot.repeated.measures.data <-
function (data.rmd, 
          x.axis = "Linear", 
          y.axis = "Linear", 
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          type = "dlb", 
          ylab = get.response.units(data.rmd),
          xlab = attr(data.rmd, "time.units"), 
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          my.title = NULL, 
          fail.level = NULL, 
          subset = T, 
          pch = 1,
          cex = 1, 
          cex.point = 0.8, 
          use.color = F, ...)
{
    response.column <- attr(data.rmd, "response.column")
    time.column <- attr(data.rmd, "time.column")
    Group <- attr(data.rmd, "Group")
    
    if (!any(class(data.rmd) == "repeated.measures.data"))
        stop(paste(deparse(substitute(data.rmd)), "is\nnot a repeated measures data set"))
    
    if (!is.data.frame(data.rmd)) {
        stop("First argument must be a degradation data object")
    }
    subset <- get.subset.vector(subset, data.rmd)
    subset.name <- attr(subset, "subset.name")
    frame.rmd <- data.rmd[subset, ]
    the.unit.column <- attr(data.rmd, "unit.column")
    Unit.marker <- frame.rmd[[the.unit.column]]
    Unit.marker <- Unit.marker[subset]
    
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.rmd), subset.name)
    }
    par(mar = c(4.5, 5, 3.5, 2) + 0.1, err = -1)
    number.unit <- unique(Unit.marker)
    
    `if`(use.color,
         col.vec <- 1:length(number.unit),
         col.vec <- rep(1, length(number.unit)))
    
   Response <- as.matrix(frame.rmd[[response.column]])
   Time <- as.matrix(frame.rmd[[time.column]])
   dimnames(Time) <- list(as.character(1:nrow(Time)), "Time")
   dimnames(Response) <- list(as.character(1:nrow(Response)),"Response")
   relationship.sanity(Time, x.axis)
   relationship.sanity(Response, y.axis)
    
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(Time)[xrna]
    
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(Response, fail.level)[yrna]
    
    plot.paper(xlim, 
               ylim, 
               x.axis = x.axis, 
               y.axis = y.axis,
               grids = F, 
               cex.tic.lab = 1)
    
    if (title.option == "full")
        title(main = my.title, cex = 0.8 * cex)
    
    title(xlab = xlab, cex.lab = 1.1)
    title(ylab = ylab, cex.lab = 1.1, mgp = c(4, 1, 0))
    
    for (i in 1:length(number.unit)) {
      
        the.ones <- Unit.marker == number.unit[i]
        Time.now <- f.relationship(Time[the.ones, ], x.axis)
        Response.now <- f.relationship(Response[the.ones, ], y.axis)
        
        group.codes <- `if`(!is.null(Group),
                            unclass(Group),
                            rep(1, length(number.unit)))
        
        if (type == "p")
            points.default(Time.now, 
                           Response.now, 
                           lty = group.codes[i],
                           pch = pch, 
                           cex = (cex.point * GetSMRDDefault("SMRD.point.size"))/100,
                           col = col.vec[i])
        
        if (type == "dl")
            points.default(Time.now, 
                           Response.now, type = "l",
                           lty = group.codes[i], 
                           pch = pch, 
                           cex = (cex.point * GetSMRDDefault("SMRD.point.size"))/100,
                           col = col.vec[i])
        if (type == "dlb")
            points.default(Time.now, 
                           Response.now, 
                           type = "b",
                           lty = group.codes[i], 
                           pch = pch, 
                           cex = (cex.point * GetSMRDDefault("SMRD.point.size"))/100,
                           col = col.vec[i])
    }
    
    if (!is.null(fail.level)) {
        abline(h = f.relationship(fail.level, y.axis), 
               lwd = 2,
               col = "blue")
    }
    invisible()
}
