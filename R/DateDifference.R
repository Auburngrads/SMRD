DateDifference <-
function (CharDateBegin, CharDateEnd, date.format = "MM/DD/YYYY")
{
    the.julianBegin <- as.POSIXct(CharDateBegin)
    the.julianEnd   <- as.POSIXct(CharDateEnd)
    as.numeric(the.julianEnd - the.julianBegin + 1)
}
