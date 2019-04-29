final.check.same.size.pv.use.conditions <-
function (plan.values.name, use.conditions) 
{
    plan.values <- get(envir = .frame0, plan.values.name)
    switch(oldClass(plan.values), ADDT.plan.values = {
        betavec <- plan.values$theta.vec[-c(1, 2, length(plan.values$theta.vec))]
    }, alt.plan.values = {
        betavec <- plan.values$betavec
        if (is.null(betavec)) stop("\nOld style plan values object---need to remake\n")
    }, {
        stop("Internal inconsistency.")
    })
    size.alt.plan.value <- length(betavec)
    size.use.conditions <- length(ClistToVec(use.conditions))
    if (map.SMRDDebugLevel() >= 4) 
        cat("\nplan value, use conditions\nsizes=", size.alt.plan.value, 
            size.use.conditions, "\n")
    if (size.use.conditions > 0 && size.alt.plan.value != size.use.conditions) {
        stop(message = paste("\n Use conditions are the wrong length.\n Number of acc vars in plan values =", 
            paste(size.alt.plan.value, ".", sep = ""), "\n Length of the use conditions = ", 
            paste(size.use.conditions, ".", sep = ""), "\nOther error messages may be generated."))
    }
    return(NULL)
}
