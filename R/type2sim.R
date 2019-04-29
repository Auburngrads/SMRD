type2sim <-
function (distribution, n, r, k = 1, number.sim = 2, kprint = 0,
   debug1= F)
{
    if (k > 1)
        accel.var.mat <- rbind(0, diag(k - 1))
    else accel.var.mat <- NULL
    altsim.out <- altsim(accel.var.mat, rep(n, k), rep(r, k),
        theta = c(rep(0, k), 1), distribution = distribution,
        number.sim = number.sim, kctype = 2, kprint = kprint,
       debug1= debug1)
    ierstuff <- altsim.out$ierstuff
    if (any(ierstuff != 0)) {
        warning("Errors in estimation detected")
        print(ierstuff[ierstuff > 0])
    }
    theta.hat <- altsim.out$theta.hat
    vcv <- altsim.out$vcv
    dimnames(theta.hat) <- list(1:nrow(theta.hat), c("mu", "sigma"))
    dimnames(vcv) <- list(1:nrow(theta.hat), c("v11", "v12",
        "v22"))
    return(list(theta.hat = theta.hat, vcv = vcv))
}
