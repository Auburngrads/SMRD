wqm.plot.nfnGroupedData <-
function (x, 
          outer = NULL, 
          inner = NULL, 
          innerGroups = NULL,
          xlab = paste(attr(x, "labels")$x, attr(x, "units")$x), 
          ylab = paste(attr(x, "labels")$y, attr(x, "units")$y), 
          strip = function(...) lattice::strip.default(..., style = 1), 
          aspect = "xy", 
          panel = function(x, y) {
                  if(Grid) lattice::panel.grid()
                  lattice::panel.xyplot(x, y)
        y.avg <- tapply(y, x, mean)
        y.avg <- y.avg[!is.na(y.avg)]
        if (length(y.avg) > 0) {
            xvals <- as.numeric(names(y.avg))
            ord <- order(xvals)
            lattice::panel.xyplot(xvals[ord], y.avg[ord], type = "l")
        }
    }, subset = T, key = TRUE, Grid = TRUE, ...)
{
    labels <- list(xlab = xlab, ylab = ylab)
    
    labels <- labels[unlist(lapply(labels, length)) > 0]
    
    args <- c(list(formula = attr(x, "formula"), 
                   data = x, 
                   strip = strip,
                   aspect = aspect, 
                   panel = panel, 
                   subset = substitute(subset)),
                   labels)
    
    if(length(outer) > 0) {
      
       if(is.logical(outer) && outer) outer <- attr(x, "outer")

       args[["formula"]][[3]][[3]] <- asOneSidedFormula(outer)[[2]]
        
       if(length(innerGroups) == 0) {
            innerGroups <- nlme::getGroupsFormula(x)
        }
    }
    if ((length(innerGroups) > 0) && (length(inner) == 0)) {
        inner <- innerGroups
        innerGroups <- NULL
    }
    
    if (length(inner) <= 0) {
      
      Inner <- NULL
      
    } else {
      
        if (is.logical(inner) && inner) {
            inner <- attr(x, "inner")
        }
      
    args[["subscripts"]] <- T
    trll.set <- lattice::trellis.par.get("superpose.line")[c("lty", "col")]
    
    if (length(innerGroups) == 0) {
      
        #args[["groups"]] <- asOneSidedFormula(inner)[[2]]
            if (missing(inner)) {
                Inner <- NULL
                trll.lty <- trll.set[["lty"]][1]
                trll.col <- trll.set[["col"]][1]
                assign(envir = .frame0,  inherits = TRUE,"trll.lty", trll.lty)
                assign(envir = .frame0,  inherits = TRUE,"trll.col", trll.col)
                
                args[["panel"]] <- function(x, y, subscripts, groups) {
                
                  if (Grid) lattice::panel.grid()
                  lattice::panel.xyplot(x, y)
                  lattice::panel.superpose(x, y, subscripts, groups, type = "l",
                    col = trll.col, lty = trll.lty)
                }
                
          } else {
            
                Inner <- as.factor(eval(asOneSidedFormula(inner)[[2]], x))
                levInn <- levels(Inner)
                args[["panel"]] <- function(x, y, subscripts, groups) {
                  
                  if (grid) lattice::panel.grid()
                  lattice::panel.xyplot(x, y)
                  lattice::panel.superpose(x, y, subscripts, groups, type = "l")
                }
          }
      
      } else {
        
            args[["groups"]] <- asOneSidedFormula(innerGroups)[[2]]
            Inner <- as.factor(eval(asOneSidedFormula(inner)[[2]], x))
            levInn <- levels(Inner)
            Inner <- (as.integer(Inner) - 1)%%length(trll.set[["lty"]]) + 1
            Grps <- as.factor(eval(asOneSidedFormula(innerGroups)[[2]], x))
            whichPars <- Inner[match(levels(Grps), Grps)]
            trll.lty <- trll.set[["lty"]][whichPars]
            trll.col <- trll.set[["col"]][whichPars]
            assign(envir = .frame0,  inherits = TRUE,"trll.lty", trll.lty)
            assign(envir = .frame0,  inherits = TRUE,"trll.col", trll.col)
            
            args[["panel"]] <- function(x, y, subscripts, groups) {
              
                if (grid) lattice::panel.grid()
                aux <- unique(sort(as.numeric(groups[subscripts])))
                lattice::panel.xyplot(x, y)
                lattice::panel.superpose(x, y, subscripts, groups, type = "l",
                  col = trll.col[aux], lty = trll.lty[aux])
            }
    }
    } 
    
    if (is.logical(key)) {
      
        if (key && (!is.null(Inner) && (lInn <- length(levInn)) > 1)) {
          
            lInn <- min(c(lInn, length(trll.set[["lty"]])))
            args[["key"]] <- list(lines = Rows(trellis.par.get("superpose.line"),
                1:lInn), text = list(levels = levInn), columns = lInn)
        }
      
    } else { args[["key"]] <- key }

    dots <- list(...)
    args[names(dots)] <- dots
    assign(envir = .frame0, inherits = TRUE, "Grid", Grid)

        args["x"] <- args["formula"]
        args["formula"] <- NULL

    xyplot <- lattice::xyplot
    do.call("xyplot", args)
}

