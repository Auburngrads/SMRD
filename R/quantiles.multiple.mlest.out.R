quantiles.multiple.mlest.out <-
function (x, prob.vec = as.numeric(ClistToVec(GetSMRDDefault("SMRD.DefaultQuantileList"))),
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, shape = NULL,
    digits = GetSMRDDefault("SMRD.DigitsPrinted"), to = 0.999,
    add.title.in = NULL,...)
{
    results <- list()
    the.names <- names(x)
    for (i in 1:length(x)) {
        if (!any(is.na(x[[i]]))) {
            if (is.null(add.title.in))
                add.title <- paste(" Group", names(x)[i],
                  sep = ";")
            else add.title <- add.title.in
            results[[the.names[i]]] <- quantiles(x[[i]],
                prob.vec = prob.vec, conf.level = conf.level,
                shape = shape, digits = digits, to = to, add.title = add.title)
        }
    }
    oldClass(results) <- c("multiple.estimates.out", "matrix")
    return(results)
}
