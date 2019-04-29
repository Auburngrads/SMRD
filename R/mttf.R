mttf <-
function (mlest.out, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100)
{
    result <- f.gendeltamethod(vcv.theta = mlest.out$vcv.matrix,
        theta = mlest.out$theta.hat, gfun = mttf.direct, distribution = mlest.out$distribution)
    ci.result <- compute.confidence.interval(param = result$vec,
        stderr = result$se, kodet = single.ifelse(is.logdist(mlest.out$distribution),
            2, 1), conf.level = conf.level)
    ci.result$title <- get.data.title(mlest.out$data.ld)
    ci.result$conf.level <- conf.level
    ci.result$distribution<-mlest.out$distribution
    ci.result$time.units<-mlest.out$time.units
    oldClass(ci.result) <- "mttf"
    return(ci.result)
}
