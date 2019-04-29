mcfdataSF <-
function (time.of.Recurr, KRecurrID, dCost, tuniq, TimeOfEnd, 
    ID.end, wcounts, rid.order.index, eid.order.index, index.end, 
   debug1= T, RecurrLimit = 100) 
{
    numRecurr <- length(time.of.Recurr)
    N <- length(TimeOfEnd)
    TEndObs <- rep(NA, numRecurr)
    j <- 1
    jndex <- eid.order.index[j]
    for (i in 1:numRecurr) {
        index <- rid.order.index[i]
        while (ID.end[jndex] < KRecurrID[index]) {
            j <- j + 1
            jndex <- eid.order.index[j]
        }
        TEndObs[index] <- TimeOfEnd[jndex]
    }
    if (numRecurr < RecurrLimit) {
        TEndObsx <- rep(NA, numRecurr)
        for (i in 1:numRecurr) {
            for (j in 1:N) {
                if (KRecurrID[i] == ID.end[j]) 
                  TEndObsx[i] <- TimeOfEnd[j]
            }
        }
        if (any(TEndObs != TEndObsx)) {
            cat("Tend disagrees\n")
            print(TEndObs)
            print(TEndObsx)
            browser()
        }
    }
    last.time <- time.of.Recurr[1]
    last.id <- KRecurrID[1]
    LnumRecurr <- 1
    if (length(time.of.Recurr) > 1) {
        for (i in 2:length(time.of.Recurr)) {
            if (KRecurrID[i] == last.id && time.of.Recurr[i] == 
                last.time) {
                dCost[LnumRecurr] <- dCost[LnumRecurr] + dCost[i]
            }
            else {
                LnumRecurr <- LnumRecurr + 1
                dCost[LnumRecurr] <- dCost[i]
            }
            KRecurrID[LnumRecurr] <- KRecurrID[i]
            TEndObs[LnumRecurr] <- TEndObs[i]
            time.of.Recurr[LnumRecurr] <- time.of.Recurr[i]
            last.id <- KRecurrID[i]
            last.time <- time.of.Recurr[i]
        }
    }
    length(time.of.Recurr) <- LnumRecurr
    length(KRecurrID) <- LnumRecurr
    length(dCost) <- LnumRecurr
    length(TEndObs) <- LnumRecurr
    mUniqRecurr <- length(tuniq)
    if (debug1) {
        print(mUniqRecurr)
        print(tuniq)
        print(LnumRecurr)
        print(cbind(KRecurrID, time.of.Recurr, dCost, TEndObs))
    }
    Apoint <- rep(NA, mUniqRecurr)
    j <- 1
    if (mUniqRecurr > 1) {
        for (i in 2:mUniqRecurr) {
            while (j <= LnumRecurr && tuniq[i] > time.of.Recurr[j]) {
                j <- j + 1
            }
            Apoint[i - 1] <- j - 1
        }
    }
    Apoint[mUniqRecurr] <- LnumRecurr
    if (LnumRecurr < RecurrLimit) {
        Apointx <- rep(0, mUniqRecurr)
        for (i in 1:mUniqRecurr) Apointx[i] <- sum(time.of.Recurr <= 
            tuniq[i])
        if (any(Apoint != Apointx)) {
            cat("Apoint disagree\n")
            print(Apoint)
            print(Apointx)
            browser()
        }
    }
    delta <- rep(NA, mUniqRecurr)
    Nback <- N
    delta[mUniqRecurr] <- 0
    for (i in 1:mUniqRecurr) {
        iup <- mUniqRecurr - i + 1
        if (i > 1) 
            delta[iup] <- delta[iup + 1]
        while (Nback > 0 && TimeOfEnd[Nback] >= tuniq[iup]) {
            if (index.end >= 0 && ID.end[Nback] != index.end) {
                delta[iup] <- delta[iup] + 1
            }
            else {
                delta[iup] <- delta[iup] + wcounts[Nback]
            }
            Nback <- Nback - 1
        }
    }
    if (LnumRecurr < RecurrLimit) {
        deltax <- rep(0, mUniqRecurr)
        for (i in 1:mUniqRecurr) deltax[i] <- sum((TimeOfEnd >= 
            tuniq[i]) * wcounts)
        if (any(delta != deltax)) {
            cat("deltaq disagree\n")
            print(delta)
            print(deltax)
            browser()
        }
    }
    return(list(tuniq = tuniq, Apoint = Apoint, delta = delta, 
        KRecurrID = KRecurrID, dCost = dCost, TEndObs = TEndObs))
}
