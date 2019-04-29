wqmf.phip <-
function (z, distribution) 
{
    switch(basic.distribution(distribution), sev = {
        p <- (1 - exp(z)) * wqmf.phis(z, distribution)
    }, normal = {
        p <- -z * wqmf.phis(z, distribution)
    }, logistic = {
        p <- wqmf.phis(z, distribution) * (1 - 2 * wqmf.phibf(z, 
            distribution))
    }, lev = {
        p <- (exp(-z) - 1) * wqmf.phis(z, distribution)
    }, {
        stop(paste(distribution, "is not a recognized distribution"))
    })
    return(p)
}
