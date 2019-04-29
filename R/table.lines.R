table.lines <-
function (z, distribution) 
{
    distribution <- basic.distribution(distribution)
    lsinf.out <- lsinf(z, "right", distribution)
    phib <- wqmf.phibf(z, distribution)
    det <- lsinf.out$f11 * lsinf.out$f22 - lsinf.out$f12^2
    v11 <- lsinf.out$f22/det
    v22 <- lsinf.out$f11/det
    v12 <- -lsinf.out$f12/det
    corr <- v12/sqrt(v11 * v22)
    vsigmagmu <- 1/lsinf.out$f22
    vmugsigma <- 1/lsinf.out$f11
    return(list(z = z, phib = phib, f11 = lsinf.out$f11, f22 = lsinf.out$f22, 
        f12 = lsinf.out$f12, v11 = v11, v22 = v22, v12 = v12, 
        rho = corr, vmugsigma = vmugsigma, vsigmagmu = vsigmagmu))
}
