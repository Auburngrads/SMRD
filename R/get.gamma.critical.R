get.gamma.critical <-
function (vcv, sigma) 
{
    fim <- solve(vcv/sigma)
    gamma.critical <- fim[2, 2] - (fim[1, 2]^2/fim[1, 1])
    pvalue <- pchisq(gamma.critical, 1)
    if (map.SMRDDebugLevel() >= 4) {
        cat(" The critical gamma is", format(gamma.critical), 
            "which corresponds to a\n", format(100 * pvalue), 
            "percent pointwise asymptotic confidence level\n")
    }
    invisible(gamma.critical)
}
