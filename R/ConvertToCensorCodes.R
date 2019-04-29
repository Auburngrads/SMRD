ConvertToCensorCodes <-
function (CensorIndicator, 
          failure.censor.names = GetSMRDDefault("SMRD.FailName"),
          right.censor.names = GetSMRDDefault("SMRD.RcName"), 
          left.censor.names = GetSMRDDefault("SMRD.LcName"),
          interval.censor.names = GetSMRDDefault("SMRD.IcName"),
          sinterval.censor.names = GetSMRDDefault("SMRD.DefaultSintervalCensorNames"),
          warn.all.ones = F, 
          warn.unrecognized = T)
{
    if (!is.numeric(CensorIndicator))
        CensorIndicator <- SMRD:::strip.blanks.factor(CensorIndicator)
    
    CensorIndicator <- casefold(CensorIndicator)
    CensorCodes <- rep(NA, length(CensorIndicator))
    
    if (is.null(failure.censor.names)) {
      
        warning("problem detected in getting censoring codes---trying to recover")
        failure.censor.names <- casefold(GetSMRDDefault("SMRD.FailName"))
        right.censor.names <- casefold(GetSMRDDefault("SMRD.RcName"))
        left.censor.names <- casefold(GetSMRDDefault("SMRD.LcName"))
        interval.censor.names <- casefold(GetSMRDDefault("SMRD.IcName"))
        sinterval.censor.names <- casefold(GetSMRDDefault("SMRD.DefaultSintervalCensorNames"))
    
        }
    
    failure.censor.names.vec   <- SMRD:::ClistToVec(failure.censor.names)
    right.censor.names.vec     <- SMRD:::ClistToVec(right.censor.names)
    left.censor.names.vec      <- SMRD:::ClistToVec(left.censor.names)
    interval.censor.names.vec  <- SMRD:::ClistToVec(interval.censor.names)
    sinterval.censor.names.vec <- SMRD:::ClistToVec(sinterval.censor.names)
    CensorCodes[SMRD:::is.onlist(CensorIndicator, sinterval.censor.names.vec)] <- 5
    CensorCodes[SMRD:::is.onlist(CensorIndicator, interval.censor.names.vec)]  <- 4
    CensorCodes[SMRD:::is.onlist(CensorIndicator, left.censor.names.vec)]      <- 3
    CensorCodes[SMRD:::is.onlist(CensorIndicator, failure.censor.names.vec)]   <- 1
    CensorCodes[SMRD:::is.onlist(CensorIndicator, right.censor.names.vec)]     <- 2


    if (warn.unrecognized && any(is.na(CensorCodes))) {
      
        na.list <- is.na(CensorCodes)
        the.list <- unique(as.character(CensorIndicator)[na.list])
        stop(paste("\nUndefined censor indicators specified---check censoring indicator name. \nLook for:",
            paste(the.list, collapse = ",")))
    }
    
    if ((!any(is.na(CensorCodes))) && warn.all.ones && all(CensorCodes ==
        1) && (!is.null(attr(CensorIndicator, "from.multiple.failure.mode"))) &&
        !attr(CensorIndicator, "from.multiple.failure.mode")) {
      
        warning("Censor column specified, but no censored observations---check censoring indicator name")
    }
    return(as.numeric(CensorCodes))
}
