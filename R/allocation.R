allocation <-
function (ADDT.test.plan) 
{
    allocation.column <- attr(ADDT.test.plan, "allocation.column")
    allocation <- ADDT.test.plan[, allocation.column, drop = F]
    col.names <- dimnames(ADDT.test.plan)[[2]]
    names(col.names) <- col.names
    dimnames(allocation)[[2]] <- col.names[allocation.column]
    return(allocation)
}
