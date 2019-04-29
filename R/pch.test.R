pch.test <-
function (x = 0:30) 
{
    plot(c(0, 1), c(0, 1), type = "n")
    for (i in 1:length(x)) {
        points.default(x.loc((i - 0.5)/length(x)), y.loc(0.5), 
            pch = x[i])
        text(x.loc((i - 0.5)/length(x)), y.loc(0.45), as.character(x[i]))
    }
}
