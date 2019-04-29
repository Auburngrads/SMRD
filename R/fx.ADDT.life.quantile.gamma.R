fx.ADDT.life.quantile.gamma <-
function (gamma.hat, p, distribution, FailLevel, xuse, transformation.response, 
    transformation.x, transformation.time, model, trans.of.quantile = F) 
{
    theta.hat <- .f.ADDT.origparam(gamma.hat, model)
    return(fx.ADDT.life.quantile(theta.hat, p, distribution, 
        FailLevel, xuse, transformation.response, transformation.x, 
        transformation.time = transformation.time, trans.of.quantile = trans.of.quantile))
}
