fx.ADDT.life.quantile <-
function (theta.hat, p, distribution, FailLevel, xuse, transformation.response, 
    transformation.x, transformation.time = transformation.time, 
    trans.of.quantile = F, dump = F) 
{
    x.tran <- xuse
    for (i in 1:ncol(xuse)) {
        x.tran[, i] <- f.relationship(xuse[, i], subscript.relationship(transformation.x, 
            i))
    }
    answer <- ADDT.life.quantile(p, distribution, f.relationship(FailLevel, 
        transformation.response), x.tran, theta.hat)
    if (dump) {
        cat("\n\ntheta.hat=", paste(theta.hat, collapse = ","), 
            "p=", p, "x=", xuse, "ans=", answer, "\ntr,tx=", 
            transformation.response, transformation.x, "in fx.ADDT.life.quantile\n")
    }
    if (!trans.of.quantile) 
        answer <- f.relationshipinv(answer, transformation.time)
    return(answer)
}
