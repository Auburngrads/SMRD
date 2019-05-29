#' @export
failure.probabilities.groupm.out <-
function (x, 
          new.data, 
          printem = T, 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          time.vec = NULL,...)
{
    `if`(!is.onlist("life.data", oldClass(x[[1]])),
         groupm.out <- x[[1]],
         groupm.out <- x)
    
    if (!is.data.frame(new.data)) new.data <- frame.new.data(new.data, groupm.out)
    
    get.single.dist.out <- get.single.dist(groupm.out, new.data = new.data)
    
    mlest.dummy <- list(distribution = groupm.out$distribution,
                        theta.hat = get.single.dist.out$thetavec, 
                        vcv = get.single.dist.out$vcv,
                        data.ld = groupm.out$data.ld)
    
    return(failure.probabilities.mlest(mlest.dummy, 
                                       add.title = get.single.dist.out$add.title,
                                       printem = printem, 
                                       conf.level = conf.level, 
                                       time.vec = time.vec,...))
}
