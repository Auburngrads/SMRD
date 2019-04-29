summary.prior.and.post <-
function (object,...)
{
    object <- fix.old.sim.post(object)
    prior <- object$prior$prior
    post <- object$post
    cat("\nPrior  -------------\n")
    cat(paste("parameters:", dimnames(prior)[2], "\n"))
    cat(paste("number points=", dim(prior)[1], "\n \n \n"))
    cat(paste("Likelihood Distribution=", object$distribution,
        "\n \n"))
    cat("Posterior  -------------\n")
    cat(paste("parameters:", dimnames(post)[2], "\n"))
    cat(paste("number points=", dim(post)[1], "\n \n \n"))
    if (any(dimnames(prior)[[2]] == "quantile")) {
        cat(paste("quantile=:", object$p, "\n"))
    }
    invisible()
}
