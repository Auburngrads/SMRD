single.quantile.var.fact <-
function (quant.of.interest, proportion.failing, distribution) 
{
    std.log.censor.time <- quant(as.numeric(proportion.failing), 
        distribution)
    std.quant.of.int <- quant(quant.of.interest, distribution)
    var.out <- ftavarvec(distribution, std.log.censor.time)
    scaled.asvar <- var.out$v11[1] + 2 * std.quant.of.int * var.out$v12[1] + 
        (std.quant.of.int)^2 * var.out$v22[1]
    return(scaled.asvar)
}
