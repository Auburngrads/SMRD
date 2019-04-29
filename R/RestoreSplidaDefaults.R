RestoreSMRDDefaults <-
function () 
{
    return(list(SMRD.ConfLevel = 95, SMRD.DigitsPrinted = 4, 
        SMRD.IcName = "Bin,Interval,bin,interval,4", SMRD.LcName = "L-Censored,Left,L-censored,left,3", 
        SMRD.RcName = "Alive,Censored,Censor,C,Noreport,R-Censored,Removed,Right,S,Survived,Survive,Suspend,Suspended,alive,censored,censor,c,NoReport,noreport,R-censored,removed,right,s,survived,survive,suspend,suspended,2", 
        SMRD.FailName = "Fail,Failed,Failure,Dead,Died,Exact,F,Report,repair,repaired,replaced,replacement,fail,failed,failure,dead,died,exact,f,report,1", 
        SMRD.DefaultSintervalCensorNames = "Sinterval,sinterval,5", 
        SMRD.DefaultQuantileList = "0.001,0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99", 
        SMRD.DateOnPlot = T, SMRD.NameOnPlot = "", SMRD.LabelOnYaxis = "Fraction Failing", 
        SMRDResponseOnXaxis = FALSE, SMRD.LongPrint = FALSE, 
        SMRD.DynamicDataObjects = FALSE, SMRD.save.def.changes = FALSE, 
        SMRD.DynamicDataObjects = FALSE, SMRD.RelatInResultsName = "always", 
        SMRD.DefaultGridPoints = 100, SMRD.Boltzmann = "eV", 
        SMRD.TitleOption = "full", SMRD.DataCheck = "strong", 
        SMRD.Randomize = "stream", SMRD.DebugLevel = "off", 
        SMRD.QuantileFocus = ".1", SMRD.solid.lines = T, 
        SMRD.point.size = 100, SMRD.long.names = T, SMRD.NumberTicks = 4, 
        SMRD.PrintSummary = T, SMRD.likelihood.method = "smoothed"))
}
