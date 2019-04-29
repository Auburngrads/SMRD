get.Costs <-
function (data.d) 
{
    cost.count.column <- get.cost.count.column(data.d)
    if (is.null(cost.count.column)) 
        the.cost.counts <- rep(1, nrow(data.d))
    else the.cost.counts <- data.d[, cost.count.column]
    return(the.cost.counts)
}
