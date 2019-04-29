get.RMD.test.plan <-
function (n, time.vec, time.units = "Time") 
{
    if (is.list(time.vec)) 
        the.plan <- list(sample.sizes = n, time.vectors = time.vec)
    else the.plan <- list(sample.sizes = n, time.vectors = list(time.vec))
    attr(the.plan, "time.units") <- time.units
    oldClass(the.plan) <- "rmd.plan"
    return(the.plan)
}
