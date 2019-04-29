print.mlehazplot.out <-
function (x, digits = 5,...)
{
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    cat(paste("\n\n", get.data.title(x$mlest.out$data.ld), "\n"))
    cat("\nMaximum likelihood hazard function estimates\n\n")
    print(x$the.table)
}
