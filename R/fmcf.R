fmcf <-
function (time, form, theta)
{
    switch(generic.nhpp.form(form, allow = F)[[1]], power.rule = {
        result <- fmcfpower(time, theta)
    }, log.linear = {
        result <- fmcfloglin(time, theta)
    }, {
        the.message <- paste("NHPP form not recognized in num.nhpp.form:",
            form)
        if (generic.nhpp.form(form, allow = F)[[2]]) {
            iform <- 0
        } else {
            stop(the.message)
        }
    })
    return(result)
}
