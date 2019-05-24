pp.quant <-
function (p, distribution, shape = NULL) 
{
    the.attributes <- attributes(p)
    
    answer <- `if`((is.null(shape) || distribution == "uniform"),
                   quant(p, distribution),
                   gquant(p, distribution, shape))
    
    attributes(answer) <- the.attributes
    return(answer)
}
