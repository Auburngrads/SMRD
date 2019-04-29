temp.profile.plot <-
function (time.vec, temperature.mat, Ea = 0.667, base.temperature = 150,
    a2.infinity = -exp(0.351), a2.zero = 0, xlab = "Hours", ylab = "Power Drop (dB)",
    base.rate = 6e-05, xrange = NULL, yrange = NULL, lwd = 1,
    add = F)
{
    par(mar = c(6.1, 7.1, 4.1, 2.1), mgp = c(4, 1, 0))
    temperature.mat <- as.matrix(temperature.mat)
    degrad.mat <- matrix(NA, nrow = length(time.vec), ncol = ncol(temperature.mat))

    af <- function (stress, stress0, coef, relationship = "Arrhenius")
      {
        warning("use AF instead, but switch argument order")
        return(exp(coef * (f.relationship(stress, relationship) -
                             f.relationship(stress0, relationship))))
      }

    for (i in 1:ncol(temperature.mat)) {
        rate.vector <- af(temperature.mat[, i], base.temperature,
            Ea, relationship = "Arrhenius3") * base.rate
        degrad.mat[, i] <- svecml1grow(times = time.vec, a2.init = a2.zero,
            a2.limit = a2.infinity, rate = 1, rate.factor = rate.vector)
    }
    if (is.null(xrange))
        xrange <- range(time.vec)
    if (is.null(yrange))
        yrange <- range(degrad.mat)
    if (!add)
        plot(xrange, yrange, type = "n", xaxs = "i", xlab = xlab,
            ylab = "", cex = 1.5, las = 1, log = "")
    title(ylab = ylab, cex = 1.5, mgp = c(5, 1, 0))
    for (i in 1:ncol(temperature.mat)) {
        lines(time.vec, degrad.mat[, i], lty = i, lwd = lwd)
    }
}
