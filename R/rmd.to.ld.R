rmd.to.ld <-
function (data.rmd, 
          fail.level, 
          x.axis = "Linear", 
          y.axis = "Linear",
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          censor.time = NULL, 
          xlab = get.time.units(data.rmd), 
          time.units = xlab,
          ylab = get.response.units(data.rmd), 
          subset = T, 
          big.one = 1e+36,
          my.title = NULL, 
          group.var = 1:length(get.x.columns(data.rmd)),
          stresses = get.x.markers(data.rmd, group.var = group.var),
          extrapolation.control = "infer",
          print.estimates = T)
{
    the.unit.column <- attr(data.rmd, "unit.column")
    Unit.marker <- data.rmd[[the.unit.column]]
    time.column <- attr(data.rmd, "time.column")
    
    if (all(oldClass(data.rmd) != "repeated.measures.data"))
        stop(paste(deparse(substitute(data.rmd)), "is not a repeated measures data set"))
    
    if (is.null(get.x.columns(data.rmd)) || any(is.na(group.var))) {
        return(single.rmd.to.ld(data.rmd, 
                                fail.level = fail.level,
                                x.axis = x.axis, 
                                y.axis = y.axis, 
                                xlim = xlim,
                                title.option = title.option, 
                                ylim = ylim, 
                                censor.time = censor.time,
                                time.units = time.units, 
                                xlab = xlab, 
                                ylab = ylab,
                                big.one = 1e+36, 
                                subset = subset, 
                                my.title = my.title,
                                print.estimates = print.estimates, 
                                extrapolation.control = extrapolation.control))
    }
    
    subset <- get.subset.vector(subset, data.rmd)
    subset.name <- attr(subset, "subset.name")
    Unit.marker <- Unit.marker[subset]
    data.rmd <- data.rmd[subset, ]
    frame.rmd <- data.rmd
    Time <- as.matrix(frame.rmd[[time.column]])
    the.response <- as.matrix(Response(data.rmd))
    dimnames(Time) <- list(as.character(1:nrow(Time)), "Time")
    dimnames(the.response) <- list(as.character(1:nrow(the.response)), "Response")
    relationship.sanity(Time, x.axis)
    relationship.sanity(the.response, y.axis)
    old.par <- par(mfrow = get.mfcol.vec(length(stresses)), 
                   oma = c(0, 4, 4, 0), 
                   err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
                                     SMRD.NameOnPlot = "")
    
    stress.names <- get.x.markers(data.rmd, 
                                   group.var = group.var,
                                   long = T)
    the.times <- NULL
    the.censor.codes <- NULL
    the.ld.xmat <- NULL
    residual.frame <- NULL
    
    for (i in 1:length(stresses)) {
      
        the.subset.data.rmd <- 
          multiple.get.rmd.subset(data.rmd,
                                  stresses[i], 
                                  columns = group.var)
        subset.data.ld <- 
          single.rmd.to.ld(the.subset.data.rmd,
                           fail.level = fail.level, 
                           x.axis = x.axis, 
                           y.axis = y.axis,
                           xlim = xlim, 
                           ylim = ylim, 
                           censor.time = censor.time,
                           time.units = time.units, 
                           xlab = xlab, 
                           ylab = ylab,
                           big.one = 1e+36, 
                           my.title = stress.names[i], 
                           doing.subset = T,
                           print.estimates = print.estimates, 
                           extrapolation.control = extrapolation.control)
        
        residual.rmd <- attr(subset.data.ld, "residual.rmd")
        residual.frame <- rbind(residual.frame, residual.rmd)
        the.times <- rbind(the.times, Response(subset.data.ld))
        the.censor.codes <- c(the.censor.codes, censor.codes(subset.data.ld))
        
        the.ld.xmat <- `if`(is.null(the.ld.xmat),
                            xmat(subset.data.ld),
                            rbind(the.ld.xmat, xmat(subset.data.ld, allow = T)))
    }
    
    SMRDOptions(save.SMRD.options)
    
    if (is.null(my.title)) {
      
        data.title <- paste(get.data.title(data.rmd), 
                            "\nwith failure defined at",
                            fail.level, ylab, 
                            subset.name)
        
       } else {
      
        data.title <- my.title
      
       }
    
    if (is.null(my.title)) my.title <- data.title

    mtext(text = my.title, side = 3, outer = T, line = 0.5, cex = 1.2)
    
    if (all(the.censor.codes == 1)) {
        the.frame <- data.frame(the.times, the.ld.xmat)
        names(the.frame) <- c("Time", get.xlabel(subset.data.ld))
        censor.column <- NULL
        
       } else {
    
        the.frame <- data.frame(the.times, the.censor.codes, the.ld.xmat)
        censor.column <- "Status"
        names(the.frame) <- c("Time", "Status", get.xlabel(subset.data.ld))
       
       }
    
    xlab <- `if`(x.axis == "Linear", 
                 get.time.units(data.rmd),
                 paste(x.axis, get.time.units(data.rmd)))
    
    residual.rmd <- frame.to.rmd(frame = residual.frame, 
                                 response.column = "Residuals",
                                 time.column = "TranTime", 
                                 time.units = xlab, 
                                 unit.column = "Unit",
                                 data.title = paste("Residuals from", get.data.title(data.rmd)),
                                 x.columns = get.x.columns(data.rmd))
    
    data.ld <- frame.to.ld(the.frame, 
                           response.column = "Time",
                           censor.column = censor.column, 
                           x.columns = get.xlabel(subset.data.ld),
                           time.units = get.time.units(subset.data.ld), 
                           xlabel = get.xlabel(subset.data.ld),
                           data.title = data.title, 
                           residual.rmd = residual.rmd,
                           data.note = get.data.note(subset.data.ld), 
                           func.call = match.call())
    return(data.ld)
}
