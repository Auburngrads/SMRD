single.rmd.to.ld <-
function (data.rmd, 
          fail.level, 
          x.axis = "Linear", 
          y.axis = "Linear",
          xlim = c(NA, NA), 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          ylim = c(NA, NA), 
          censor.time = NULL, 
          time.units = get.time.units(data.rmd),
          xlab = get.time.units(data.rmd), 
          ylab = attr(data.rmd, "response.units"), 
          subset = T,
          big.one = 1e+36, 
          my.title = NULL, 
          doing.subset = F, 
          extrapolation.control = "infer",
          print.estimates = T)
{
    response.units <- attr(data.rmd, "response.units")
    response.column <- attr(data.rmd, "response.column")
    time.column <- attr(data.rmd, "time.column")
    the.x.columns <- get.x.columns(data.rmd)
    unit.column <- attr(data.rmd, "unit.column")
    Unit.marker <- data.rmd[[unit.column]]
    xlabel <- attr(data.rmd, "xlabel")

    add.to.data.frame <- 
      function (input.frame, 
                new.object, 
                new.name = deparse(substitute(new.object)))
      {
        if (is.null(new.object)) {
          
          return(input.frame)
          
        } else {
          
          the.names <- c(names(input.frame), new.name)
          input.frame <- data.frame(input.frame, new.object)
          names(input.frame) <- the.names
          return(input.frame)
        }
      }

    if (all(oldClass(data.rmd) != "repeated.measures.data")) {
      
        stop(paste(deparse(substitute(data.rmd)), 
                   "is not a repeated measures data set"))
    }
    
    subset <- get.subset.vector(subset, data.rmd)
    subset.name <- attr(subset, "subset.name")
    Unit.marker <- Unit.marker[subset]
    frame.rmd <- data.rmd[subset, ]
    
    if (is.null(frame.rmd) || !is.data.frame(frame.rmd)) {
        stop("First argument must be a degradation data object")
    }
    
    Response <- as.matrix(frame.rmd[[response.column]])
    Time <- as.matrix(frame.rmd[[time.column]])
    
    `if`(is.null(my.title),
         data.title <- paste(get.data.title(data.rmd), 
                             "with failure defined at",
                             fail.level, ylab, subset.name),
         data.title <- my.title)
    
    if (is.null(my.title)) my.title <- data.title
    
    unique.units <- unique(Unit.marker)
    number.unique.units <- length(unique.units)
    times <- rep(NA, length(unique.units))
    unit.name <- rep("", length(unique.units))
    censor.codes <- rep(1, length(unique.units))
    
    `if`(is.null(the.x.columns),
         the.xmat <- NULL,
         the.xmat <- frame.rmd[names(the.x.columns)])
    
    if (!is.null(the.xmat)) {
        ncol.the.xmat <- ncol(the.xmat)
        xmat.ld <- the.xmat[1:length(unique.units), , drop = F]
    }
    
    plotem <- T
    xplotmat <- matrix(NA, ncol = length(unique.units), nrow = 3)
    yplotmat <- matrix(NA, ncol = length(unique.units), nrow = 3)
    increasing <- rep(NA, length(unique.units))
    firsty <- rep(NA, length(unique.units))
    time.firsty <- rep(NA, length(unique.units))
    tran.fail.level <- f.relationship(fail.level, y.axis)
    bad.list <- NULL
    residual.frame <- NULL
    wrong.direction <- NULL
    
    if (print.estimates) cat("\n")
    
    for (i in 1:length(unique.units)) {
      
        if (!is.null(the.xmat)) {
            sub.xmat <- as.data.frame(the.xmat[Unit.marker ==
                unique.units[i], , drop = F])
            xmat.ld[i, ] <- sub.xmat[1, , drop = F]
        }
      
        sub.frame <- frame.rmd[which(Unit.marker == unique.units[i]), ]
        y <- as.matrix(f.relationship(sub.frame[[response.column]],
            y.axis))
        x <- f.relationship(sub.frame[[time.column]], x.axis)
        minx <- min(x)
        maxx <- max(x)
        left.censored <- F
        
        switch(extrapolation.control, increasing = {
            if (y[1] > fail.level) left.censored <- T
        }, decreasing = {
            if (y[1] < fail.level) left.censored <- T
        }, infer = {
        })
        
        if (left.censored) {
            times[i] <- x[1]
            censor.codes[i] <- 3
            next
        }
        if (length(x) < 2) {
            bad.list <- c(bad.list, unique.units[i])
            next
        }
        
        crossing <- approx(y, 
                           x, 
                           xout = tran.fail.level, 
                           method = "linear",
                           rule = 1)$y
        
        if (!is.na(crossing)) {
          
            trans.fail.time <- crossing
            
      } else {
        
            ls.fit <- lm(y ~ x, singular.ok = T)
            residual.response <- residuals(ls.fit)
            sub.residual.frame <- 
              data.frame(Residuals = residual.response,
                         TranTime = x, 
                         Unit = sub.frame[[unit.column]])
            
            if (!is.null(the.x.columns))
                sub.residual.frame <- 
              add.to.data.frame(sub.residual.frame,
                                as.data.frame(sub.frame[, names(the.x.columns)]),
                                xlabel)
            
            residual.frame <- rbind(residual.frame, 
                                    sub.residual.frame)
            
            ls.estimates <- coefficients(ls.fit)
            
            `if`(doing.subset && !is.null(my.title),
                 group.mark <- my.title,
                 group.mark <- "")
            
            if (print.estimates)
                cat("Regression estimates for unit", group.mark,
                  unique.units[i], paste(c("intercept", "slope"),
                    "=", format(ls.estimates), collapse = " and "),
                  "\n")
            trans.fail.time <- (tran.fail.level - ls.estimates[1])/ls.estimates[2]
            
            `if`(trans.fail.time > 0,
                 max.plot.x <- trans.fail.time,
                 max.plot.x <- maxx)
            
            if (!is.null(censor.time))
                max.plot.x <- min(max.plot.x, f.relationship(censor.time,
                  x.axis))
            midx <- (minx + maxx)/2
            resp.minx <- ls.estimates[1] + ls.estimates[2] * minx
            resp.midx <- ls.estimates[1] + ls.estimates[2] * midx
            resp.maxx <- ls.estimates[1] + ls.estimates[2] * max.plot.x
            xplotmat[, i] <- c(minx, midx, max.plot.x)
            yplotmat[, i] <- c(resp.minx, resp.midx, resp.maxx)
      }
        
        increasing[i] <- y[1] < tran.fail.level
        firsty[i] <- y[1]
        time.firsty[i] <- x[1]
        times[i] <- f.relationshipinv(trans.fail.time, x.axis)
        right.censored <- F
        
        if (!is.na(times[i])) {
            if (times[i] < 0)
                right.censored <- T
            if (!is.null(censor.time) && times[i] > censor.time)
                right.censored <- T
        }
        
        switch(extrapolation.control, increasing = {
            if (!increasing[i]) right.censored <- T
            wrong.direction <- c(wrong.direction, i)
        }, decreasing = {
            if (increasing[i]) right.censored <- T
            wrong.direction <- c(wrong.direction, i)
        }, infer = {
        })
        
        if (right.censored) {
            if (is.null(censor.time))
                stop(paste("Sample paths in wrong direction for unit ",
                  Unit.marker[i], "; must specify censoring time.",
                  sep = ""))
            times[i] <- censor.time
            censor.codes[i] <- 2
        }
    }
    
    if (!is.null(bad.list)) {
        cat(paste("\nWarning: Some units had only one reading:\n",
            paste(bad.list, collapse = ","), "\n\n"))
    }
    
    good.ones <- is.onlist(unique.units, bad.list)
    need.to.drop <- is.na(yplotmat[1, ]) | is.na(times)
    if (any(!need.to.drop)) {
        yplotmat <- yplotmat[, !need.to.drop, drop = F]
        xplotmat <- xplotmat[, !need.to.drop, drop = F]
    }
    increasing <- strip.na(increasing)
    if (length(increasing) > 0 && !(all(increasing) | all(!increasing))) {
        warning("Some paths increasing and some paths decreasing")
        the.bad.frame <- 
          data.frame(firsty = f.relationshipinv(firsty, y.axis), 
                     fail.level = rep(fail.level, number.unique.units),
                     first.time = f.relationshipinv(time.firsty, x.axis),
                     increasing = increasing)
        print(the.bad.frame)
    }
    times <- as.matrix(times)
    if (plotem) {
        xrna <- is.na(xlim)
        if (any(xrna)) {
          
            `if`(!is.null(censor.time),
                 below.censor.time <- xplotmat <= f.relationship(censor.time, x.axis),
                 below.censor.time <- T)
          
            `if`(length(xplotmat) > 0,
                 xplotsub <- xplotmat[xplotmat < big.one & below.censor.time],
                 xplotsub <- NULL)
            
            xlim[xrna] <- range(f.relationshipinv(xplotsub,
                x.axis), Time, censor.time, strip.na(times))[xrna]
        }
        
        yrna <- is.na(ylim)
        if (any(yrna)) {
          
            `if`(length(yplotmat) > 0,
                 yplotsub <- yplotmat[yplotmat < big.one],
                 yplotsub <- NULL)
          
            ylim[yrna] <- range(Response, 
                                fail.level, 
                                f.relationshipinv(yplotsub, y.axis), 
                                frame.rmd[[response.column]])[yrna]
        }
        
        single.plot.repeated.measures.data(data.rmd, 
                                           xlim = xlim,
                                           ylim = ylim, 
                                           x.axis = x.axis, 
                                           y.axis = y.axis,
                                           ylab = ylab, 
                                           xlab = xlab, 
                                           my.title = my.title, 
                                           fail.level = fail.level,
                                           subset = subset)
        
        if (!is.null(censor.time)) {
          
            abline(v = f.relationship(censor.time, x.axis), 
                   col = 1,
                   lwd = 2)
            axis(side = 3, 
                 tck = -0.045, 
                 labels = F, 
                 lwd = 2, 
                 at = censor.time)
            mtext(side = 3, 
                  parse(text = "Censor~Time%->%''"), 
                  adj = 1, at = censor.time)
        }
        
        if (length(xplotmat) > 0)
            matlines(xplotmat, yplotmat, lwd = 2)
    }
    
    dimnames(times) <- list(rep(NULL, nrow(y)), time.units)
    
    if (!is.null(the.xmat)) {
      
        xmat.ld <- as.data.frame(xmat.ld[!is.na(times), ])
        xlabel <- names(the.xmat)
        
  } else {
    
        xlabel <- NULL
        xmat.ld <- NULL
  }
    
    censor.codes <- strip.na(censor.codes, times)
    times <- as.data.frame(strip.na(times))
    names(times) <- "Time"
    
    if (all(censor.codes == 1)) {
      
        censor.codes <- NULL
        censor.column <- NULL
        
  } else {
    
        censor.column <- "Status"
  }
    
    xlab <- single.ifelse(x.axis == "Linear", 
                          get.time.units(data.rmd),
                          paste(x.axis, get.time.units(data.rmd)))
    
    residual.rmd <- frame.to.rmd(frame = residual.frame, 
                                 response.column = "Residuals",
                                 time.column = "TranTime", 
                                 time.units = xlab, 
                                 unit.column = "Unit",
                                 data.title = paste("Residuals from", get.data.title(data.rmd)),
                                 x.columns = the.x.columns)
    
    the.frame <- add.to.data.frame(times, censor.codes, "Status")
    the.frame <- add.to.data.frame(the.frame, xmat.ld, xlabel)
    dimnames(the.frame) <- list(unique.units, dimnames(the.frame)[[2]])
    
    data.ld <- frame.to.ld(the.frame, response.column = "Time",
        censor.column = censor.column, x.columns = xlabel, time.units = time.units,
        xlabel = xlabel, data.title = data.title, residual.rmd = residual.rmd,
        data.note = get.data.note(data.rmd), func.call = match.call())
    
    return(data.ld)
}
