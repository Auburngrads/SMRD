print.estimates.out <-
function (x, digits = 5,...)
{
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    cat("\n\n", attr(x, "title"), sep = "")
    distribution <- attr(x, "distribution")
    if (is.null(distribution))
        distribution <- attr(x, "mlest.out")$distribution
    if (length(distribution) == 1)
        cat("\n", distribution, "Distribution\n\n")
    else {
        names.distribution <- dimnames(distribution)
        dimnames(distribution) <- list(as.character(1:nrow(distribution)),
            names.distribution[[2]])
        print.data.frame(as.data.frame(distribution), quote = F)
    }

        xx <- x
        oldClass(xx) <- "matrix"
        dim(xx) <- dim(x)
        dimnames(xx) <- dimnames(x)
        attr(xx, "distribution") <- NULL
        attr(xx, "title") <- NULL
        attr(xx, "mlest.out") <- NULL
        attr(xx, "class") <- NULL
        print(xx)

    invisible(x)
}
