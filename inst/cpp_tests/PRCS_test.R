library(SMRD)

distribution = 'exponential'
n = 100
r = 10
k = 10
d.range = c(0.01, 2)
d.length = 100
dvec = seq(d.range[1], d.range[2],
           length = d.length)
number.sim = 10
kprint = 0
debug1 = F

altsim.out <- SMRD:::altsim(rbind(0, diag(k - 1)),
                     rep(n, k), rep(r, k),
                     theta = c(rep(0, k), 1),
                     distribution = distribution,
                     number.sim = number.sim,
                     kctype = 2,
                     kprint = kprint,
                     debug1 = debug1)
ierstuff <- altsim.out$ierstuff
if (any(ierstuff != 0)) {
  warning("Errors in estimation detected")
  print(ierstuff[ierstuff > 0])
}
theta.hat <- as.matrix(altsim.out$theta.hat[, -(k + 1)])
theta.hat <- cbind(theta.hat[, 1, drop = F],
                   theta.hat[,-1, drop = F] + theta.hat[, 1])
  if (k > 2) {
    zmax <- apply(theta.hat[, -1], 1, "max")
  } else {
    zmax <- theta.hat[, -1]
  }

zout <- .Fortran("prcs",
                 zmax = as.single(zmax),
                 z1 = as.single(theta.hat[,1]),
                 nsim = as.integer(nrow(theta.hat)),
                 dvec = as.single(dvec),
                 answer = single(length(dvec)),
                 as.integer(length(dvec)))

old.return <- list(dvec = zout$dvec,
                   prob = zout$answer)

cout <- SMRD2::PRCS(zmax, 
                    z1 = theta.hat[,1], 
                    nsim = nrow(theta.hat),
                    dvec = dvec, 
                    answer = double(length(dvec)),
                    nd = length(dvec))

new.return <- list(dvec = cout$dvec,
                   prob = cout$answer)

old.return$dvec - new.return$dvec
old.return$prob - new.return$prob
