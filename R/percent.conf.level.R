percent.conf.level <-
function (conf.levels)
{
    paste(format(100 * conf.levels), "%", sep = "")
}
