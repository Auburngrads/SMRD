get.ALT.test.plan.subframe <-
function (frame, levels.columns, plan.rows) 
{
    if (!is.data.frame(frame)) 
        frame <- get(envir = .frame0, frame)
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    levels.columns <- check.column(levels.columns, ncol.data.mat, 
        names.the.frame, number.col.allowed = -1)
    plan.columns <- c(levels.columns, "number.units", "censor.times")
    return.frame <- frame[sort(as.numeric(plan.rows)), plan.columns, 
        drop = F]
    attr(return.frame, "allocation.column") <- attr(frame, "allocation.column")
    attr(return.frame, "accelvar.names") <- attr(frame, "accelvar.names")
    attr(return.frame, "levels.columns") <- attr(frame, "levels.columns")
    attr(return.frame, "describe.string") <- attr(frame, "describe.string")
    return(return.frame)
}
