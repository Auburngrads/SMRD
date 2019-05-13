#' @export
unfold.alt.plan <-
function (x, alt.plan.values, use.condition,...)
{
    accel.variable <- attr(x, "accelvar.names")
    number.of.units <- x[, "number.units"]
    total.units <- sum(number.of.units)
    the.levels <- attr(x, "levels")
    accel.variable.levels <- x[, accel.variable]
    relationship <- alt.plan.values$relationship
    distribution <- alt.plan.values$distribution
    if (length(alt.plan.values$theta.vec) != 3)
        stop(paste("length of theta.vec is", length(alt.plan.values$theta.vec),
            "\nIt should be length 3.\n"))
    beta0 <- alt.plan.values$theta.vec["beta0"]
    beta1 <- alt.plan.values$theta.vec["beta"]
    sigma <- alt.plan.values$theta.vec["sigma"]
    pi <- number.of.units/total.units
    xstress <- f.relationship(accel.variable.levels, relationship)
    xd <- f.relationship(use.condition, relationship)
    xh <- f.relationship(max(accel.variable.levels), relationship)
    xi <- (xstress - xd)/(xh - xd)
    eta <- max(logb(strip.na(x[, "censor.times"])))
    if (is.na(eta) || is.null(eta)) {
        eta <- (beta0 + beta1 * max(xstress)) + quant(0.999999999,
            distribution) * sigma
    }
    a <- (eta - (beta0 + beta1 * xd))/sigma
    b1 <- (beta1 * (xh - xd))/sigma
    pd <- wqmf.phibf(a, distribution)
    ph <- wqmf.phibf(a - b1, distribution)
    mud <- eta - a * sigma
    muh <- mud + b1 * sigma
    zeta <- a - b1 * xi
    pfail <- wqmf.phibf(a - b1 * xi, distribution)
    return(list(accel.variable = accel.variable, number.of.units = number.of.units,
        total.units = total.units, the.levels = the.levels, accel.variable.levels = accel.variable.levels,
        relationship = relationship, distribution = distribution,
        beta0 = beta0, beta1 = beta1, sigma = sigma, pi = pi,
        xstress = xstress, xd = xd, xh = xh, xi = xi, eta = eta,
        a = a, b1 = b1, pd = pd, ph = ph, mud = mud, muh = muh,
        zeta = zeta, pfail = pfail))
}
