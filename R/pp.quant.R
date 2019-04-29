pp.quant <-
function (p, distribution, shape = NULL) 
{
    the.attributes <- attributes(p)
    
    `if`(is.null(shape) || distribution == "uniform",
         answer <- quant(p, distribution),
         answer <- gquant(p, distribution, shape))
    
    attributes(answer) <- the.attributes
    return(answer)
}
