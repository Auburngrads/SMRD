print.ADDT.test.plan <-
function (x, print = "hframe",...)
{
    frame.to.print <- switch(print, default = {
        x
    }, vframe = {
        if (attr(x, "frame.type") == "hframe") frame.to.print <- x else frame.to.print <- hframe.to.vframe(x)
    }, hframe = {
        if (attr(x, "frame.type") == "vframe") {
            frame.to.print <- vframe.to.hframe(x)
        } else {
            the.hframe <- hframe.to.vframe(x)
            frame.to.print <- vframe.to.hframe(the.hframe)
        }
    }, {
        frame.to.print <- x
    })
    print.data.frame(frame.to.print)
    cat("\nTotal number of units in the plan =", sum(frame.to.print),
        "\n")
    invisible()
}
