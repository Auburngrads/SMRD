logplot <-
function (x, y)
{
    plot.paper(range(x), range(y), x.axis = "log", y.axis = "log",
        grids = F)
    points.default(logb(x), logb(y))
}
