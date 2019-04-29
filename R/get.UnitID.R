get.UnitID <-
function (data.d) 
{
    UnitID.column <- get.UnitID.column(data.d)
    if (is.null(UnitID.column)) 
        stop("Internal error SMRD---cannot find UnitIDs")
    the.UnitIDs <- data.d[, UnitID.column]
    return(the.UnitIDs)
}
