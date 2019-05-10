pp.quant <-
function (p, distribution, shape = NULL) 
{
    the.attributes <- attributes(p)
    
    answer <- `if`((is.null(shape) || distribution == "uniform"),
                   SMRD2:::quant(p, distribution),
                   SMRD2:::gquant(p, distribution, shape))
    
    attributes(answer) <- the.attributes
    return(answer)
}
