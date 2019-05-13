#' @export
summary.multiple.mlest.out <-
function (object, weibull.traditional = T, sum.likelihood = T,
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, digits = GetSMRDDefault("SMRD.DigitsPrinted"),...)
{
    group.names <- names(object)
    if (any(group.names == generic.distribution("weibull")))
        weibull.traditional <- F
    if (any(oldClass(object) == "mfm.multiple.mlest.out") ||
        !is.null(c(generic.distribution(group.names[[1]], allow = T),
            generic.distribution(group.names[[length(group.names)]],
                allow = T))))
        sum.likelihood <- F
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    result.table <- matrix(rep(NA, length(object) * 5),
        nrow = length(object))
    dist.vec <- rep(NA, length(object))
    for (i in 1:length(object)) {
        mlest.out <- object[[i]]
        if (!is.list(mlest.out) && is.na(mlest.out)) {
            next
        }
        extra.stuff <- attr(mlest.out, "extra.stuff")
        data.ld <- mlest.out$data.ld
        if (!is.null(extra.stuff$stresses))
            xlabel <- get.xlabel(data.ld)
        else {
            xlabel <- "Xlabel"
        }
        distribution <- mlest.out$distribution
        dist.vec[i] <- distribution
        if (is.null(distribution))
            next
        the.distribution <- distribution
        time.units <- get.time.units(data.ld)
        the.title <- extra.stuff$main.title
        stresses <- extra.stuff$stresses
        group.var <- extra.stuff$group.var
        theta.hat <- mlest.out$theta.hat
        se.musig <- sqrt(diag(mlest.out$vcv.matrix))
        if (weibull.traditional && .distnum(numdist(distribution)) ==
            "Weibull") {
            eta <- exp(theta.hat[1])
            beta <- 1/theta.hat[2]
            result.table[i, ] <- c(mlest.out$log.likelihood,
                eta, eta * se.musig[1], beta, beta^2 * se.musig[2])
      } else {
            result.table[i, ] <- c(mlest.out$log.likelihood,
                theta.hat[1], se.musig[1], theta.hat[2], se.musig[2])
        }
    }
    dist.vec <- strip.na(dist.vec)
    the.distribution <- dist.vec[1]
    result.frame <- data.frame(names(object), result.table)
    if (is.R())
        group.var.label <- "Group"
    else group.var.label <- paste(xlabel[group.var], collapse = ", ")
    if (weibull.traditional && .distnum(numdist(the.distribution)) ==
        "Weibull") {
        the.dimnames <- list(as.character(1:length(object)),
            c(group.var.label, "Log likelihood", "eta", "se_eta",
                "beta", "se_beta"))
  } else {
        the.dimnames <- list(as.character(1:length(object)),
            c(group.var.label, "Log likelihood", "mu", "se_mu",
                "sigma", "se_sigma"))
    }
    the.dimnames <- list(as.character(1:length(object)),
        c(group.var.label, "Log likelihood", "mu", "se_mu", "sigma",
            "se_sigma"))
    dimnames(result.frame) <- the.dimnames
    if (length(unique(dist.vec)) > 1) {
        if (all(is.na(match(vector.generic.distribution(dist.vec),
            vector.generic.distribution(group.names, allow = T)))))
            warning(paste("\nNon-unique dist-vec", paste(dist.vec,
                collapse = ",")))
    }
    cat(paste("\n", the.title, "\n"))
    cat("\nMaximum likelihood estimation results:\n")
    cat(paste("Response units: ", time.units, "\n", sep = ""))
    cat(paste("\n", distribution.name(the.distribution), " Distribution\n",
        sep = ""))
    print(result.frame)
    if (sum.likelihood)
        cat(paste("\nTotal log likelihood=", format(sum(strip.na(result.frame[,
            2]))), "\n"))
    invisible(result.frame)
}
