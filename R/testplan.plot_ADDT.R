testplan.plot_ADDT<-
function (ADDT.test.plan)
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

    ADDT.test.plan <- hframe.to.vframe(ADDT.test.plan)
    the.allocations <- allocation(ADDT.test.plan)
    the.times <- times(ADDT.test.plan)
    the.levels <- AT.levels(ADDT.test.plan)
    if (ncol(the.levels) > 1) {
        cat("No plot for multiple variable plans\n")
        invisible(NULL)
    }
    plot.paper(xlim = range(the.times), ylim = range(the.levels[[1]]),
        x.axis = "linear", y.axis = "linear", ylab = names(the.levels),
        xlab = dimnames(the.times)[2][1], cex = 1.5, cex.labs = 1.5,
        grids = F)
    ntimes <- ncol(the.times)
    text((the.times[, 1] + the.times[, ntimes])/2, the.levels[[1]],
        the.allocations[[1]])
    invisible()
}
