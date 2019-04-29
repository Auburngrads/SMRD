thin.mcf <-
function (mcf.out, number = 300) 
{
    the.ones <- seq(1, length(mcf.out$tuniq), by = max(floor(length(mcf.out$tuniq)/number), 
        1))
    mcf.out$tuniq <- mcf.out$tuniq[the.ones]
    mcf.out$muHat <- mcf.out$muHat[the.ones]
    mcf.out$VarHat <- mcf.out$VarHat[the.ones]
    mcf.out$risk.set <- mcf.out$risk.set[the.ones]
    return(mcf.out)
}
