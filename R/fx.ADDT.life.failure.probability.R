fx.ADDT.life.failure.probability <-
function (theta.hat, time.vec, distribution, FailLevel, xuse, 
    transformation.response, transformation.x, transformation.time) 
{
    time.vec.tran <- f.relationship(time.vec, transformation.time)
    x.tran <- xuse
    for (i in 1:ncol(xuse)) {
        x.tran[, i] <- f.relationship(xuse[, i], subscript.relationship(transformation.x, 
            i))
    }
    answer <- ADDT.life.failure.probability(time.vec.tran, distribution, 
        f.relationship(FailLevel, transformation.response), x.tran, 
        theta.hat)
    return(answer)
}
