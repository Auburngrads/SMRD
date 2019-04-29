summary.posterior <-
function (object,...)
{
    prior <- object$prior
    post <- object$post
    cat("\nPosterior object for", get.data.title(object$data.ld),
        "\n")
    cat("\nPrior distribution specifications -------------")
    print.prior(object$specifications.for.prior)
    cat("\nPrior distribution sample -------------\n")
    cat(paste("parameters:", dimnames(prior)[2], "\n"))
    cat(paste("number points=", dim(prior)[1], "\n \n "))
    cat(paste("Likelihood Distribution=", object$distribution,
        "\n \n"))
    cat("Posterior sample -------------\n")
    cat(paste("parameters:", dimnames(post)[2], "\n"))
    cat(paste("number points=", dim(post)[1], "\n \n \n"))
    invisible()
}
