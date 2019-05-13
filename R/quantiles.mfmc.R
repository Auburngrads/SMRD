#' @export
quantiles.mfmc <-
function (x, prob.vec = as.numeric(ClistToVec(GetSMRDDefault("SMRD.DefaultQuantileList"))),
    my.title = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    digits = GetSMRDDefault("SMRD.DigitsPrinted"), band.type = "pointwise",
    add.title = NULL,...)
{
    data.ld <- attr(x, "data.ld")
    if (any(prob.vec >= 1) || any(prob.vec <= 0)) {
        warning("Probabilities for quantiles outside 0-1 removed from list.")
        prob.vec <- prob.vec[prob.vec > 0 & prob.vec < 1]
    }
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    distribution.vec <- unlist(lapply(x, function(sublist) {
        sublist[["distribution"]]
    }))
    if (length(unique(distribution.vec)) > 1) {
        distribution <- cbind(names(x), distribution.vec)
        dimnames(distribution) <- list(rep("", length = nrow(distribution)),
            c("Failure Mode", "Distribution"))
    }
    else {
        distribution <- distribution.vec[1]
    }
    conf.char <- percent.conf.level(conf.level)
    conf.int.title <- paste("Pointwise Approximate", conf.char,
        "Confidence Intervals")
    my.title <- paste("Combined Multiple Failure Mode Series System\nParametric ML Quantile Estimates from ",
        get.data.title(data.ld), add.title, "\n\n", conf.int.title,
        "\n", sep = "")
    time.range <- f.mfmc.bound.quantiles(x,
        prob.vec)
    if (generic.band.type(band.type) == "pointwise") {
        cat("\nComputing confidence intervals.\n")
        mfmc.quantiles.out <- f.mfmc.quantiles(x,
            prob.vec, time.range = time.range, do.se = T)
        cat("\nFinished computing confidence intervals.\n")
        log.of.data <- all(unlist(lapply(x,
            function(sublist) {
                is.logdist(sublist[["distribution"]])
            })))
        if (log.of.data)
            kodet.vec <- rep(2, length(mfmc.quantiles.out$vec))
        else kodet.vec <- rep(1, length(mfmc.quantiles.out$vec))
        the.list <- compute.confidence.interval(mfmc.quantiles.out$vec,
            mfmc.quantiles.out$se, kodet = kodet.vec, conf.level = conf.level)
    }
    else {
        mfmc.quantiles.out <- f.mfmc.quantiles(x,
            prob.vec, time.range = time.range, do.se = F)
        the.list <- list(fun.hat = mfmc.quantiles.out)
    }
    the.table <- cbind(p = prob.vec, Quanhat = the.list$fun.hat,
        Stderror = the.list$se.fun, Lower = the.list$fun.lower,
        Upper = the.list$fun.upper)
    if (is.null(the.table))
        return(NULL)
    if (ncol(the.table) == 5) {
        dimnames(the.table) <- list(rep(" ", nrow(the.table)),
            c("p", "Quanhat", "Std.Err.", paste(conf.char, "Lower"),
                paste(conf.char, "Upper")))
    }
    else {
        dimnames(the.table) <- list(rep(" ", nrow(the.table)),
            c("p", "Quanhat"))
    }
    attr(the.table, "title") <- my.title
    attr(the.table, "distribution") <- distribution
    attr(the.table, "mlest.out") <- x
    oldClass(the.table) <- c("quantiles.out", "estimates.out", "matrix")
    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}
