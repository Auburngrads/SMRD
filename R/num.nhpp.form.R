num.nhpp.form <-
function (form, allow = F)
{
    switch(generic.nhpp.form(form, allow = allow)[[1]], power.rule = iform <- 1,
        log.linear = iform <- 2, {
            the.message <- paste("NHPP form not recognized in num.nhpp.form:",
                form)
            if (generic.nhpp.form(form, allow = F)[[2]]) {
                iform <- 0
            } else {
                stop(the.message)
            }
        })
    return(iform)
}
