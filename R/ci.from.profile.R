ci.from.profile <-
function (struct1, confidence.level = 0.95)
{
    is.max <- function (x) { x == max(x) }
    hvalue <- exp(Uminus(qchisq(confidence.level, 1))/2)
    ci <- c(NA, NA)
    struct1.y <- struct1$y
    struct1.x <- struct1$x
    length.vec <- length(struct1.y)
    max.logical <- is.max(struct1.y)
    the.max.index <- (1:length.vec)[max.logical]
    if (length(the.max.index) > 1) {
        flat.xlim <- range(struct1.x[the.max.index])
        the.max.index <- the.max.index[1]
        warning(paste("\nProfile likelihood appears flat at",
            struct1.y[the.max.index], "over", paste(flat.xlim,
                collapse = " and "), "\n"))
    }
    lower <- (1:length.vec) < the.max.index
    if (struct1.y[1] < hvalue && the.max.index > 1 && sum(lower) >=
        2) {
        ci[1] <- approx(struct1.y[lower], struct1.x[lower], xout = hvalue)$y
    }
    if (struct1.y[length.vec] < hvalue && the.max.index < length.vec &&
        sum(!lower) >= 2) {
        ci[2] <- approx(struct1.y[!lower], struct1.x[!lower],
            xout = hvalue)$y
    }
    if (any(is.na(ci)))
        cat("There were problems finding the confidence interval from the computed likelihood profile\n")
    return(ci)
}
