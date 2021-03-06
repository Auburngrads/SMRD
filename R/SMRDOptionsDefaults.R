#' Title
#'
#' @param ... 
#'
#' @return NULL
#' @export
SMRDOptionsDefaults <-
  function (...)
  {
    return(list(SMRD.ConfLevel = 95, 
                SMRD.DigitsPrinted = 4,
                SMRD.FailName = "event,exact,d,dead,died,f,fail,failed,failure,report,repair,repaired,replaced,replacement,1",
                SMRD.RcName = "a,alive,c,censor,censored,end,mend,noreport,r,r-censored,right-censored,removed,right,rightcensored,s,survived,survive,suspend,suspended,2",
                SMRD.LcName = "l,l-censored,left-censored,left,leftcensored,start,mstart,3",
                SMRD.IcName = "b,bin,i,interval,i-censored,intervalcensored,interval-censored,4",
                SMRD.DefaultSintervalCensorNames = "s,sinterval,smallinterval,small-interval,5",
                SMRD.DefaultQuantileList = "0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99",
                SMRD.DateOnPlot = FALSE, 
                SMRD.NameOnPlot = "", 
                SMRD.LabelOnYaxis = "Fraction Failing",
                SMRDResponseOnXaxis = FALSE, 
                SMRD.LongPrint = TRUE,
                SMRD.DynamicDataObjects = FALSE, 
                SMRD.save.def.changes = FALSE,
                SMRD.DynamicDataObjects = FALSE, 
                SMRD.RelatInResultsName = "always",
                SMRD.DefaultGridPoints = 100, 
                SMRD.Boltzmann = "eV",
                SMRD.TitleOption = "blank", 
                SMRD.DataCheck = "strong",
                SMRD.Randomize = "stream", 
                SMRD.DebugLevel = "off",
                SMRD.QuantileFocus = ".1", 
                SMRD.solid.lines = TRUE,
                SMRD.point.size = 100, 
                SMRD.point.pch = rep(c(15:18,3,4),10),
                SMRD.long.names = TRUE, 
                SMRD.NumberTicks = 4,
                SMRD.PrintSummary = TRUE, 
                SMRD.likelihood.method = "smoothed",
                SMRD.ClassCoding = "contr.treatment", 
                SMRD.WarnMaskedObjects = TRUE,
                SMRD.ConfidenceBandMethod = "zhat.cdf.method"))
  }
