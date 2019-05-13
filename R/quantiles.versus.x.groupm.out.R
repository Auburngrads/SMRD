#' @export
quantiles.versus.x.groupm.out <-
function (x, new.data, prob, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,...)
{
    `if`(is.onlist("life.data", oldClass(x[[1]])),
         groupm.out <- x,
         groupm.out <- x)
  
    answers <- rep(NA, length = nrow(new.data))
    
    for (i in 1:nrow(new.data)) {
      
        get.single.dist.out <- 
          get.single.dist(groupm.out, 
                          new.data = new.data[i, , drop = F])
        
        mlest.dummy <- list(distribution = groupm.out$distribution,
            theta.hat = get.single.dist.out$thetavec, vcv = get.single.dist.out$vcv,
            data.ld = groupm.out$data.ld)
        
        answers[i] <- get.parametric.quantiles(mlest.dummy, prob.vec = prob,
            do.ci = F)
    }
    
    invisible(answers)
}
