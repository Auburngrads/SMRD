CheckPrintDataName <-
function () 
{
    the.par <- par(xpd = T)
    on.exit(par(the.par))
    if (!is.R()) {
        if (SMRDOptions("SMRD.DateOnPlot")) {
            text(x.loc(1), y.loc(-0.13), date(), adj = 1, cex = 0.8)
        }
        text(x.loc(1), y.loc(-0.16), GetSMRDDefault("SMRD.NameOnPlot"), 
            adj = 1, cex = 0.8)
    }
    else {
        if (SMRDOptions("SMRD.DateOnPlot")) {
            text(x.loc(1), y.loc(-0.2), date(), adj = 1, cex = 0.8)
        }
        text(x.loc(1), y.loc(-0.23), GetSMRDDefault("SMRD.NameOnPlot"), 
            adj = 1, cex = 0.8)
    }
}
