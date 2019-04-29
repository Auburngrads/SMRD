plotRiskSet <-
function (data.rdu, xlab = paste("Time in", get.time.units(data.rdu)),
    ylab = ifelse(proportion, "Proportion at Risk", "Number in Risk Set"),
    proportion = F, plot.average = T, my.title = NULL, JustEvent = F)
{
    theRiskSet <- RiskSet(data.rdu, JustEvent = JustEvent)
    time.diff <- diff(theRiskSet$Times)
    if (proportion) {
        the.response <- theRiskSet$Counts/theRiskSet$NumberUnits
        the.average <- sum(time.diff * theRiskSet$Counts[-1])/sum(theRiskSet$NumberUnits *
            time.diff)
    }
    else {
        the.response <- theRiskSet$Counts
        the.average <- sum(time.diff * theRiskSet$Counts[-1])/sum(time.diff)
    }
    plot.paper(range(theRiskSet$Times), range(the.response),
        xlab = xlab, ylab = ylab, grids = F)
    if (is.null(my.title))
        my.title <- get.data.title(data.rdu)
    mtext(text = my.title, side = 3, line = 4, cex = 1.2)
    num <- length(theRiskSet$Times)
    segments(theRiskSet$Times[-num], the.response[-num], theRiskSet$Times[-1],
        the.response[-num])
    segments(theRiskSet$Times, c(0, the.response[-num]), theRiskSet$Times,
        the.response)
    if (plot.average)
        abline(h = the.average)
    invisible()
}
