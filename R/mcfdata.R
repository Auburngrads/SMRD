mcfdata <-
function (time.of.Recurr, KRecurrID, dCost, tuniq, time.of.end, 
    ID.end, counts,debug1= T) 
{
    last.time <- time.of.Recurr[1]
    last.id <- KRecurrID[1]
    LnumRecurr <- 1
    cat(date(), "length(time.of.Recurr)", length(time.of.Recurr), 
        "\n")
    for (i in 2:length(time.of.Recurr)) {
        if (i%%100 == 0) 
            cat(date(), "Collapse recurrence", i, "\n")
        if (KRecurrID[i] == last.id && time.of.Recurr[i] == last.time) {
            dCost[LnumRecurr] <- dCost[LnumRecurr] + dCost[i]
        }
        else {
            LnumRecurr <- LnumRecurr + 1
            dCost[LnumRecurr] <- dCost[i]
        }
        KRecurrID[LnumRecurr] <- KRecurrID[i]
        time.of.Recurr[LnumRecurr] <- time.of.Recurr[i]
        last.id <- KRecurrID[i]
        last.time <- time.of.Recurr[i]
    }
    cat(date(), "LnumRecurr=", LnumRecurr, "\n")
    length(time.of.Recurr) <- LnumRecurr
    length(KRecurrID) <- LnumRecurr
    length(dCost) <- LnumRecurr
    mUniqRecurr <- length(tuniq)
    N <- length(time.of.end)
    Apoint <- rep(0, mUniqRecurr)
    for (i in 1:mUniqRecurr) {
        if (i%%100 == 0) 
            cat(date(), "pointer recurrence", i, "\n")
        Apoint[i] <- sum(time.of.Recurr <= tuniq[i])
    }
    cat(date(), "Pointers done\n")
    w <- counts + (ID.end != 0)
    delta <- rep(0, mUniqRecurr)
    for (i in 1:mUniqRecurr) {
        if (i%%100 == 0) 
            cat(date(), "delta recurrence", i, "\n")
        delta[i] <- sum((time.of.end >= tuniq[i]) * w)
    }
    cat(date(), "N (num end points, j upper)=", N, "\n")
    TEndObs <- rep(0, LnumRecurr)
    for (i in 1:LnumRecurr) {
        if (i%%100 == 0) 
            cat(date(), "TEnd recurrence i=", i, "\n")
        for (j in 1:N) {
            if (j%%100 == 0) 
                cat(date(), "TEnd recurrence i,j=", i, j, "\n")
            if (KRecurrID[i] == ID.end[j]) {
                TEndObs[i] <- time.of.end[j]
            }
        }
    }
    cat(date(), "TEnd done\n")
    return(list(tuniq = tuniq, Apoint = Apoint, delta = delta, 
        KRecurrID = KRecurrID, dCost = dCost, TEndObs = TEndObs))
}
