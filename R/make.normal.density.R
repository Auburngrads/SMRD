make.normal.density <-
function () 
{
    tmp <- basic.get.density.lines(density.at = 0.5, mu = 0.5, 
        sigma = 0.1, distribution = "normal", scale.factor = 0.5, 
        x.axis = "linear")
    plot(tmp$y, -tmp$x, type = "n")
    lines(tmp$y, -tmp$x, type = "l", lwd = 7)
}
