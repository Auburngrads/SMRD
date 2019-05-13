#' @export
quantiles.groupm.out <-
function (x, new.data, prob.vec = as.numeric(ClistToVec(GetSMRDDefault("SMRD.DefaultQuantileList"))),
    printem = T, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    to = 0.999, digits = GetSMRDDefault("SMRD.DigitsPrinted"),...)
{
    `if`(is.onlist("life.data", oldClass(x[[1]])),
         groupm.out <- x[[1]],
         groupm.out <- x)
    
    if (!is.data.frame(new.data))
        new.data <- frame.new.data(new.data, groupm.out)
    
    get.single.dist.out <- get.single.dist(groupm.out, new.data = new.data)
    mlest.dummy <- list(distribution = groupm.out$distribution,
        theta.hat = get.single.dist.out$thetavec, vcv = get.single.dist.out$vcv,
        data.ld = groupm.out$data.ld)
    
    return(quantiles.mlest(mlest.dummy, add.title = get.single.dist.out$add.title,
        printem = printem, conf.level = conf.level, prob.vec = prob.vec,
        ...))
}
