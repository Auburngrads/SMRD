#' Title
#'
#' @param x 
#' @param conf.level 
#' @param digits 
#' @param print.vcv 
#' @param add.title 
#' @param quote 
#' @param prefix 
#' @param ... 
#'
#' @return printed stuff
#' @export
print.gmle.out <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    digits = GetSMRDDefault("SMRD.DigitsPrinted"), print.vcv = GetSMRDDefault("SMRD.LongPrint"),
    add.title = NULL, quote = T, prefix = "",...)
{
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    cat(paste("\n\n", get.data.title(x$data.ld), add.title,
        "\n"))
    the.analysis.type <- x$analysis.type
    if (!is.null(the.analysis.type))
        cat("\n", the.analysis.type, "\n", sep = "")
    response.units <- get.response.units(x$data.ld)
    if (response.units != "")
        cat(paste("Response units: ", response.units, "\n",
            sep = ""))
    time.units <- get.time.units(x$data.ld)
    if (time.units != "" && time.units != response.units)
        cat(paste("\nTime units: ", time.units, "\n", sep = ""))
    the.distribution <- x$model$distribution
    if (!is.null(the.distribution))
        the.distribution <- distribution.name(x$model$distribution)
    if (!is.null(the.distribution))
        cat("\n", the.distribution, " Distribution\n", sep = "")
    the.relationship <- x$relationship
    if (!is.null(the.relationship))
        cat(paste("\nRelationship:", the.relationship, "\n"))
    cat("\nMaximum likelihood estimation results\n")
    if (!is.null(x$est.out$converged) && x$est.out$converged)
        result <- "\nAppears to have converged;"
    else result <- "\nDid not converge;"
    cat(result, x$est.out$conv.type, " \n")
    cat("\nLog likelihood at maximum point:", format(x$max.log.like),
        "\n \n")
    if (print.vcv || is.null(x$origparam)) {
        cat(" Gradiant of likelihood evaluated at stopping point\n")
        print(x$grad)
        cat(" \n \n Transformed parameter ML estimates:\n")
        print(x$t.param)
        cat("\n Transformed parameter ML estimate hessian matrix:\n")
        print(x$hessian)
        cat("\n Transformed parameter ML estimate variance-covariance matrix\n")
        print(x$t.vcv)
        cat("\n Transformed parameters  ML estimate correlation matrix\n")
        param.corr <- ccor(x$t.vcv)
        dimnames(param.corr) <- dimnames(x$t.vcv)
        print(param.corr)
        if (!is.null(x$fixed.param.list$fixed.parameters)) {
            fixed.parameters <- x$fixed.param.list$fixed.parameters
            param.corr <- param.corr[-fixed.parameters, -fixed.parameters,
                drop = F]
            sub.names <- dimnames(x$t.vcv)[[1]][-fixed.parameters]
            dimnames(param.corr) <- list(sub.names, sub.names)
        }
        if (!any(is.na(param.corr)) && !any(is.nan(param.corr))) {
            if (ncol(param.corr) > 1) {
                cat("\n Eigen structure of the transformed parameters  ML estimate correlation matrix:\n")
                print(eigen(param.corr))
            }
        }
        else {
            cat("Problems detected in the correlattion matrix:\n")
            print(param.corr)
        }
        cat("Standard errors for transformed parameter MLEs:\n")
        std.errors <- sqrt(diag(x$t.vcv))
        names(std.errors) <- dimnames(x$t.vcv)[[1]]
        print(std.errors)
    }
    if (!is.null(x$origparamvcv)) {
        se.orig <- sqrt(diag(x$origparamvcv))
        names(se.orig) <- dimnames(x$origparamvcv)[[1]]
        the.ci <- compute.confidence.interval(x$origparam,
            se.orig, x$kodet, conf.level = conf.level)
        esti.matrix <- cbind(x$origparam, se.orig, the.ci$fun.lower,
            the.ci$fun.upper)
        conf.char <- percent.conf.level(conf.level)
        dimnames(esti.matrix)[2] <- list(c("MLE", "Std.Err.",
            paste(conf.char, "Lower"), paste(conf.char, "Upper")))
        dimnames(esti.matrix)[2] <- list(c("MLE", "Std.Err.",
            paste(conf.char, "Lower"), paste(conf.char, "Upper")))
        print(esti.matrix)
        param.corr <- ccor(x$origparamvcv)
        if (print.vcv) {
            cat("\n parameter ML estimate variance-covariance matrix\n")
            print(x$origparamvcv)
            cat("\n parameter ML estimate correlation matrix\n")
            dimnames(param.corr) <- dimnames(x$origparamvcv)
            print(param.corr)
            if (!is.null(x$fixed.param.list$fixed.parameters)) {
                fixed.parameters <- x$fixed.param.list$fixed.parameters
                param.corr <- param.corr[-fixed.parameters, -fixed.parameters,
                  drop = F]
                sub.names <- dimnames(x$t.vcv)[[1]][-fixed.parameters]
                dimnames(param.corr) <- list(sub.names, sub.names)
            }
        }
        if (!any(is.na(param.corr)) && !any(is.nan(param.corr))) {
            if (ncol(param.corr) > 1) {
                eigen.out <- eigen(param.corr)
                if (print.vcv) {
                  cat("\n Eigen structure of the parameter ML estimate correlation matrix:\n")
                  print(eigen.out)
                }
                if (any(eigen.out$values < 0)) {
                  the.message <- "\nNegative eigenvalues of the correlation matrix---suggests convergence problems.\nOther Problems may arise."
                  warning(the.message)
                }
            }
        }
        else {
            cat("Problems detected in the correlattion matrix:\n")
            print(param.corr)
        }
    }
    if (!is.null(x$thetainterp)) {
        cat("\n  Interpretation parameter MLEs \n")
        print(x$thetainterp)
    }
    invisible()
    return(esti.matrix)
}
