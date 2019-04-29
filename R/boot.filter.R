boot.filter <-
function (boot.results) 
{
    ierstuff <- boot.results$ierstuff
    start.length <- length(ierstuff)
    theta.hat.star <- as.matrix(boot.results$theta.hat.star)
    the.uniques <- unique(apply(theta.hat.star, 1, paste, sep = "", 
        collapse = ","))
    cat("\n There were ", length(the.uniques), "unique bootstrap samples out of\n", 
        start.length, "total bootstrap samples\n")
    boot.results$theta.hat.star <- theta.hat.star[ierstuff == 
        0, ]
    boot.results$vcv <- boot.results$vcv[ierstuff == 0, ]
    boot.results$likelihood <- boot.results$likelihood[ierstuff == 
        0]
    boot.results$ierstuff <- boot.results$ierstuff[ierstuff == 
        0]
    end.length <- length(boot.results$ierstuff)
    if (start.length > end.length) {
        the.percent <- 100 * ((start.length - end.length)/start.length)
        cat("\n A total of", start.length - end.length, "of", 
            start.length, paste("(", the.percent, "%)", sep = ""), 
            "bad bootstrap samples\n were removed because of convergence difficulties.\n These were probably samples with 0 failures \n or no variability in the response.\n\n")
    }
    invisible(boot.results)
}
