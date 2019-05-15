#' Title
#'
#' @param pi.vec 
#' @param xi.vec 
#' @param distribution 
#' @param a 
#' @param b1 
#' @param b2 
#' @param pd 
#' @param ph 
#' @param theta 
#' @param theta.known 
#' @param quad.model 
#' @param perc 
#' @param zd 
#' @param kprint 
#' @param levels 
#' @param cex2 
#' @param xlab 
#' @param ylab 
#' @param scale.var 
#' @param old.a 
#' @param old.b1 
#' @param xiL.lower 
#' @param dump 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' varone.grid(a = -0.9, 
#'             b1 = -7.9, 
#'             perc = 0.01, 
#'             xlab = "xiL", 
#'             ylab = "piL",
#'             xiL.lower = -1, 
#'             levels = c(.001,.01,0.10000000000000001, 0.20000000000000001, 0.5, 1, 2),
#'             dump = T)
#' 
#' }
varone.grid <-
function (pi.vec = seq(0.001, 0.999, length = 100), xi.vec = seq(xiL.lower,
    0.999, length = 100), distribution = "weibull", a = NULL,
    b1 = NULL, b2 = 0, pd = NULL, ph = NULL, theta = 1, theta.known = T,
    quad.model = F, perc = 0.01, zd = 0, kprint = 0, levels = c(0.1,
        0.2, 0.5, 1, 2), cex2 = 2, xlab = "X", ylab = "Y", scale.var = T,
    old.a = NULL, old.b1 = NULL, xiL.lower = 0.001, dump = F)
{
    if (!is.null(ph) && !is.null(pd)) {
        a <- logb(-logb(1 - pd))
        b1 <- a - theta * logb(-logb(1 - ph))
        old.b1 <- -b1
        old.a <- a + old.b1
  } else {
        if (!is.null(old.a) && !is.null(old.b1)) {
            b1 <- -old.b1
            a <- old.a - old.b1
            pd <- psev(a)
            ph <- psev(old.a)
      } else {
            if (is.null(a))
                stop("Need to specify a")
            old.b1 <- -b1
            old.a <- a + old.b1
            pd <- psev(a)
            ph <- psev(old.a)
        }
    }
    if (dump)
        cat("pd=", pd, "ph=", ph, "\n")
    if (dump)
        cat("a =", a, "b1 =", b1, "    old.a =", old.a, "old.b1 =",
            old.b1, "\n")
    tmp1.vvaronevar <- function(xi.vec, pi.vec, distribution,
        a, b1, b2 = 0, theta = 1, theta.known = T, quad.model = F,
        perc, zd = 0, kprint = 0) {
        return(vvaronevar(distribution, a, b1, b2, theta, theta.known,
            quad.model, perc, xi.vec, zd, pi.vec, kprint))
    }
    zmat <- outer(xi.vec, pi.vec, tmp1.vvaronevar, distribution,
        a, b1, b2, theta, theta.known, quad.model, perc, zd,
        kprint)
    if (dump)
        cat("Minimum, Maximum variance=", min(zmat[zmat > 0]),
            max(zmat), "\n")
    if (scale.var)
        zmat <- log10(zmat/min(zmat))
    if (!is.null(levels)) {
        contour(xi.vec, pi.vec, zmat, levels = levels, cex = 1.5,
            xlab = "", ylab = "", pty = "s")
  } else {
        contour(xi.vec, pi.vec, zmat, cex = 1.5, xlab = "", ylab = "",
            las = 1, pty = "s")
    }
    if (names(dev.cur()) == "postscript") {
        mixed.mtext(side = 2, texts = ylab, cex = cex2, line = -2,
            srt = 0)
        mixed.mtext(side = 1, texts = xlab, cex = cex2, line = 3)
  } else {

        mtext(side = 2, text = ylab, cex = cex2, line = 2)
        mtext(side = 1, text = xlab, cex = cex2, line = 3)
    }
    invisible(zmat)
}
