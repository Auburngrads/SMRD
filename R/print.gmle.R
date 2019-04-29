print.gmle <-
function (x,...)
{
    warning(paste("use print.gmle.out instead of deprecated print.gmle"))
    print.gmle.out(x)
    invisible()
}
