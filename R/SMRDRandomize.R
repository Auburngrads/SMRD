SMRDRandomize <-
function () 
{
    number <- as.numeric(substring(date(), first = 16, last = 16)) * 
        100 + as.numeric(substring(date(), first = 18, last = 19))
    junk <- runif(number)
    invisible(junk)
}
