fmcfdiff <-
function (timel, timeu, form, theta) 
{
    if (all(timel <= 0)) 
        fmcfdiff <- fmcf(timeu, form, theta)
    else fmcfdiff <- fmcf(timeu, form, theta) - fmcf(timel, form, 
        theta)
    return(fmcfdiff)
}
