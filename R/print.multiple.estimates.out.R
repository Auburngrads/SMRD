print.multiple.estimates.out <-
function (x,...)
{
    for (i in 1:length(x)) {
        print(x[[i]])
    }
}
