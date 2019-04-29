StripPeriods <-
function (periodstring) 
{
    paste(unlist(wqm.unpaste(periodstring, sep = ".")), collapse = " ")
}
