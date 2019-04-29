FixXrangeYrange <-
function (func.call, which = 1) 
{
    if (which == 2) {
        func.call$x.min <- func.call$x.min2
        func.call$x.max <- func.call$x.max2
        func.call$y.min <- func.call$y.min2
        func.call$y.max <- func.call$y.max2
    }
    xlim <- c(eval(func.call$x.min), eval(func.call$x.max))
    func.call$xlim <- xlim
    ylim <- c(eval(func.call$y.min), eval(func.call$y.max))
    func.call$ylim <- ylim
    return(func.call)
}
