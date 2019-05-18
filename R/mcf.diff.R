mcf.diff <-
function (data1.rdu, 
          data2.rdu, 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100) 
{
    norquan <- qnorm(1 - (1 - conf.level)/2)
    mcf1.out <- mcf(data1.rdu)
    mcf2.out <- mcf(data2.rdu)
    maxcen <- min(mcf1.out$max.censoring.time, mcf2.out$max.censoring.time)
    utime <- c(mcf1.out$tuniq, mcf2.out$tuniq)
    order.vec <- order(utime)
    utime <- utime[order.vec]
    sys.ind <- c(rep(1, length(mcf1.out$tuniq)), rep(2, length(mcf2.out$tuniq)))[order.vec]
    muhat <- c(mcf1.out$muHat, mcf2.out$muHat)[order.vec]
    varxmu <- c(mcf1.out$VarHat, mcf2.out$VarHat)[order.vec]
    sysnow1 <- 0
    sysnow2 <- 0
    var.sysnow1 <- 0
    var.sysnow2 <- 0
    xmudiffhat <- muhat
    se.xmudiffhat <- muhat
    for (i in 1:length(utime)) {
        inow <- i
        if (utime[i] > maxcen) 
            break
        if (sys.ind[i] == 1) {
            sysnow1 <- muhat[i]
            var.sysnow1 <- varxmu[i]
        }
        else {
            sysnow2 <- muhat[i]
            var.sysnow2 <- varxmu[i]
        }
        xmudiffhat[i] <- sysnow1 - sysnow2
        se.xmudiffhat[i] <- sqrt(var.sysnow1 + var.sysnow2)
    }
    length(xmudiffhat) <- inow - 1
    length(utime) <- inow - 1
    length(se.xmudiffhat) <- inow - 1
    ucl <- xmudiffhat + norquan * se.xmudiffhat
    lcl <- xmudiffhat - norquan * se.xmudiffhat
    return.list <- list(utime = utime, xmudiffhat = xmudiffhat, 
        se.xmudiffhat = se.xmudiffhat, lcl = lcl, ucl = ucl, 
        maxcen = maxcen)
    return(return.list)
}
