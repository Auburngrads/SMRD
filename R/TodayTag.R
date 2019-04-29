TodayTag <-
function (include.time = T) 
{
    the.date <- date()
    Month <- substring(the.date, 5, 7)
    Day <- substring(the.date, 9, 10)
    the.time <- my.strip.blanks(substring(the.date, 12, 19), 
        StripChar = ":", FillChar = "")
    nc <- nchar(the.date)
    Year <- substring(the.date, nc - 3, nc)
    Tag <- paste(Day, Month, Year, sep = "")
    if (include.time) 
        Tag <- paste(Tag, the.time, sep = ".")
}
