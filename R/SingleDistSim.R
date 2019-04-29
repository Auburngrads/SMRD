SingleDistSim <-
function (number.sim, distribution, sample.size, censor.type = "None",
    fail.number = NULL, fail.fraction = NULL, theta = c(0, 1),
   debug1= F, kprint = 0, randomize = T, remove.bad = F, var = T)
{
    func.call <- match.call()
    distribution <- generic.distribution(distribution)
    switch(censor.type, `Type I` = , `Type 1` = {
        kctype <- 1
        censor.type <- "right"
        if (is.null(fail.fraction)) stop("Must provide fail.fraction for Type 1 censoring")
        if (is.logdist(distribution)) centim <- exp(theta[1] +
            theta[2] * quant(fail.fraction, distribution)) else centim <- theta[1] +
            theta[2] * quant(fail.fraction, distribution)
        zquant <- quant(fail.fraction, distribution)
    }, `Type II` = , `Type 2` = {
        kctype <- 2
        censor.type <- "right"
        if (is.null(fail.number)) stop("Must provide fail.number for Type 1 censoring")
        centim <- fail.number
        if (fail.number == sample.size) zquant <- 100 else {
            zquant <- quant(fail.number/sample.size, distribution)
        }
    }, None = , none = {
        kctype <- 2
        zquant <- 100
        censor.type <- "uncensored"
        centim <- sample.size
    }, {
        stop(paste(censor.type, "is unrecognized censor type"))
    })
    CensorInfo <- list(censor.type = censor.type, zquant = zquant)
    accel.var.mat <- NULL
    altsim.out <- altsim(accel.var.mat = accel.var.mat, nsamsz = sample.size,
        centim = centim, theta = theta, distribution = distribution,
        number.sim = number.sim, kctype = kctype, kprint = kprint,
       debug1= debug1, randomize = T)
    theta.hat <- altsim.out$theta.hat
    vcvobs <- altsim.out$vcv
    data.problems <- altsim.out$ierstuff > 0
    if (any(data.problems)) {
        if (remove.bad) {
            theta.hat <- theta.hat[!data.problems, ]
            vcvobs <- vcvobs[!data.problems, ]
      } else {
            theta.hat[data.problems, ] <- NA
            vcvobs[data.problems, ] <- NA
        }
    }
    dimnames(theta.hat) <- list(1:nrow(theta.hat), c("mu", "sigma"))
    if (var) {
        dimnames(vcvobs) <- list(1:nrow(theta.hat), c("v11",
            "v12", "v22"))
        lsinf.out <- lsinf(z = zquant, censor.type = censor.type,
            distribution = distribution)
        the.det <- lsinf.out$f11 * lsinf.out$f22 - lsinf.out$f12^2
        V <- c(lsinf.out$f22, -lsinf.out$f12, lsinf.out$f11)/the.det
        vcvexp <- outer(altsim.out$theta.hat[, 2]^2/sample.size,
            V)
        dimnames(vcvexp) <- list(1:nrow(theta.hat), c("v11",
            "v12", "v22"))
        return(list(theta.hat = theta.hat, vcvobs = vcvobs, vcvexp = vcvexp,
            ier = altsim.out$ierstuff, call = func.call))
  } else {
        the.det <- altsim.out$vcv[, 1] * altsim.out$vcv[, 3] -
            altsim.out$vcv[, 2]^2
        infobs <- cbind(altsim.out$vcv[, 3], -altsim.out$vcv[,
            2], altsim.out$vcv[, 1])/the.det
        dimnames(infobs) <- list(1:nrow(theta.hat), c("I11",
            "I12", "I22"))
        lsinf.out <- lsinf(z = zquant, censor.type = censor.type,
            distribution = distribution)
        Info <- c(lsinf.out$f11, lsinf.out$f12, lsinf.out$f22)
        infexp <- outer(sample.size/altsim.out$theta.hat[, 2]^2,
            Info)
        dimnames(infexp) <- list(1:nrow(theta.hat), c("I11",
            "I12", "I22"))
        return(list(theta.hat = theta.hat, infobs = infobs, infexp = infexp,
            ier = altsim.out$ierstuff, call = func.call, CensorInfo = CensorInfo))
    }
}
