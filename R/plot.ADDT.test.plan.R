#' @export
plot.ADDT.test.plan <-
function (x,which,...)
{
  AT.levels <-
    function (ADDT.test.plan)
    {
      levels.columns <- attr(ADDT.test.plan, "levels.columns")
      levels <- ADDT.test.plan[, levels.columns, drop = F]
      col.names <- dimnames(ADDT.test.plan)[[2]]
      names(col.names) <- col.names
      dimnames(levels) <- list(as.character(1:nrow(levels)), col.names[levels.columns])
      oldClass(levels) <- "data.frame"
      return(levels)
    }

    x <- hframe.to.vframe(x)
    the.allocations <- allocation(x)
    the.times <- times(x)
    the.levels <- AT.levels(x)
    
    if (ncol(the.levels) > 1 && (missing(which) || is.null(which))) {
        cat("No plot created for multiple variable plans.\n\nProceeding with first variable:", colnames(the.levels)[1],"\n")
        invisible(NULL)
    }
    
    plot.paper(xlim = range(the.times), 
               ylim = range(the.levels[[which]]),
               x.axis = "linear", 
               y.axis = "linear", 
               ylab = names(the.levels)[which],
               xlab = dimnames(the.times)[2][1], 
               cex = 1.5, 
               cex.labs = 1.5,
               grids = F)
    
    ntimes <- ncol(the.times)
    
    text((the.times[, 1] + the.times[, ntimes]) / 2, 
         the.levels[[which]],
         the.allocations[[1]])
    
    invisible()
}
