get.life.data.class <-
function (data.ld)
{
    mfms.mark <- NULL
    characters.mark <- NULL
    alts.mark <- NULL
    regrs.mark <- NULL
    factors.mark <- NULL
    number.characters <- 0
    number.factors <- 0
    number.alts <- 0
    number.regrs <- 0
    if (!is.null(failure.modes(data.ld)) && length(unique(failure.modes(data.ld))) >
        1)
        mfms.mark <- "mfm"
    if (!is.null(xmat(data.ld, allow = T))) {
        for (i in 1:ncol(xmat(data.ld))) {
            xnow <- xmat(data.ld)[, i]
            if (is.factor(xnow))
                number.factors <- number.factors + 1
            if (is.character(xnow))
                number.characters <- number.characters + 1
            if (is.numeric(xnow)) {
                if (length(unique(xnow)) > 10)
                  number.regrs <- number.regrs + 1
                else number.alts <- number.alts + 1
            }
        }
    }
    if (number.characters > 0)
        characters.mark <- paste("characters", number.factors,
            sep = "")
    if (number.factors > 0)
        factors.mark <- paste("factors", number.factors, sep = "")
    if (number.alts > 0)
        alts.mark <- paste("alts", number.alts, sep = "")
    if (number.regrs > 0)
        regrs.mark <- paste("regrs", number.regrs, sep = "")
    qualifier <- paste(mfms.mark, characters.mark, factors.mark,
        alts.mark, regrs.mark, sep = "")
    basic.class <- c("life.data", "data.frame")
    if (length(qualifier) > 0) {
        qualifier <- paste(qualifier, ".", sep = "")
        return(c(paste(qualifier, "life.data", sep = ""), basic.class))
    }
    else (return(basic.class))
}
