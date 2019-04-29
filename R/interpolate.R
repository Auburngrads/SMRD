interpolate <-
function (x, y, mark, xtarget) 
{
    return(y[mark] + ((y[mark + 1] - y[mark]) * (xtarget - x[mark]))/(x[mark + 
        1] - x[mark]))
}
