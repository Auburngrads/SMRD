fx.ADDT.degradation.quantile <-
function (theta.hat, p, distribution, xuse, time.vec, transformation.time, 
    transformation.x) 
{
    x.tran <- xuse
    for (i in 1:ncol(xuse)) {
        x.tran[, i] <- f.relationship(xuse[, i], subscript.relationship(transformation.x, 
            i))
    }
    answer <- ADDT.degradation.quantile(p, distribution, x.tran, 
        f.relationship(time.vec, transformation.time), theta.hat)
    return(answer)
}
