final.check.same.size.pv.test.plan <-
function (test.plan.name, plan.values.name) 
{
    test.plan <- get(envir = .frame0, test.plan.name)
    plan.values <- get(envir = .frame0, plan.values.name)
    switch(oldClass(plan.values), ADDT.plan.values = {
        betavec <- plan.values$theta.vec[-c(1, 2, length(plan.values$theta.vec))]
    }, alt.plan.values = {
        betavec <- plan.values$betavec
        if (is.null(betavec)) stop("\nOld style plan values object---need to remake\n")
    })
    levels.columns <- attr(test.plan, "levels.columns")
    if (is.null(levels.columns)) 
        stop("*******Old-style test plan---need to remake*******")
    size.alt.plan.value <- length(betavec)
    size.alt.test.plan <- length(levels.columns)
    if (map.SMRDDebugLevel() >= 4) 
        cat("\nplan value, test plan\nsizes=", size.alt.plan.value, 
            size.alt.test.plan, "\n")
    if (size.alt.plan.value != size.alt.test.plan) {
        stop(message = paste("\n Incompatible plan values and test plan chosen.\n Number of acc vars in plan values =", 
            paste(size.alt.plan.value, ".", sep = ""), "\n Number of acc vars in test plan = ", 
            paste(size.alt.test.plan, ".", sep = ""), "\nOther error messages may be generated."))
    }
    return(NULL)
}
