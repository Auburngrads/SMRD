failure.probabilities.multiple.mlest.out <-
function (x, time.vec = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    shape = NULL, digits = GetSMRDDefault("SMRD.DigitsPrinted"),
    add.title.in = NULL,...)
{
    results <- list()
    the.names <- names(x)
    for (i in 1:length(x)) {
        if (!any(is.na(x[[i]]))) {
            if (is.null(add.title.in))
                add.title <- paste(" Group", names(x)[i])
            else add.title <- add.title.in
            results[[the.names[i]]] <- failure.probabilities(x[[i]],
                time.vec = time.vec, conf.level = conf.level,
                shape = shape, digits = digits, add.title = add.title)
        }
    }
    oldClass(results) <- "multiple.estimates.out"
    return(results)
}
