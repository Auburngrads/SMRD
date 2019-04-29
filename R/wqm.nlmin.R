wqm.nlmin <-
function (log.like, theta.start, dump, max.fcal = 900, max.iter = 150,
    rfc.tol = 1e-10, xc.tol = 1e-10)
{
    est.out <- optim(par = theta.start, fn = log.like, method = "BFGS",
            control = list(maxit = max.iter, reltol = rfc.tol))
        if (est.out$convergence > 0)
            warning("Optim convergence problem code=", est.out$convergence)
        results <- list(x = est.out$par, converge = est.out$convergence)

    return(results)
}
