cix <-
function (data.ld) 
{
    cat("\nNames in frame:\n")
    print(names(data.ld))
    cat("\nx.cols of frame:\n")
    print(attr(data.ld, "x.columns"))
    cat("\nfirst two lines of the data frame:\n")
    print.data.frame(data.ld[1:2, ])
    cat("\nattributes of frame:\n")
    print(names(attributes(data.ld)))
    invisible()
}
