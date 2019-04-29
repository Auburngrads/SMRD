generic.nhpp.form <-
function (form, allow = F)
{
    if (!is.character(form))
        stop(paste("form must be character string:", form))
    switch(casefold(form), `power law` = , power.law = , powerlaw = ,
        `power rule` = , powerrule = , power.rule = {
            form <- "power.rule"
        }, `log linear` = , loglinear = , log.linear = {
            form <- "log.linear"
        }, {
            if (allow) {
                return(NULL)
            } else {
                stop(paste(form, "is unrecognized form in generic.nhpp.form()"))
            }
        })
    return(list(form,allow))
}
