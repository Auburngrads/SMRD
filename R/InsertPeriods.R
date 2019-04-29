.InsertPeriods <-
function (periodstring)
{
    if (is.null(periodstring))
        return(NULL)
    paste(unlist(wqm.unpaste(periodstring, sep = " ")), collapse = ".")
}
