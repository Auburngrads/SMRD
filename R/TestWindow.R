TestWindow <-
function (ID, time, event, count) 
{
    if (SMRD:::map.SMRDDebugLevel() > 0) {
        cat("ID\n")
        print(ID)
        cat("time\n")
        print(time)
        cat("event\n")
        print(event)
        cat("count\n")
        print(count)
    }
    SeriousProblem <- F
    naID <- is.na(ID)
    if (any(naID)) {
        stop("NAs appear in the unit ID column")
        if (sum(naID) < 20) {
            bad.ones <- (1:length(ID))[naID]
            print(bad.ones)
        }
    }
    case.old.event <- casefold(as.character(event))
    uniqueIDorig <- unique(ID)
    uniqueID <- sort(unique(ID))
    EndPoints <- is.element(case.old.event, c("end", "mend", "2"))
    SEndPoints <- is.element(case.old.event, c("end"))
    StartPoints <- is.element(case.old.event, c("start", "mstart"))
    SStartPoints <- is.element(case.old.event, c("start"))
    MultiplePoints <- is.element(case.old.event, c("mstart", "mend"))
    
    if (sum(StartPoints) > sum(EndPoints)) {
        warning(paste("The number of start points", sum(StartPoints), 
            "is greater than the number of end points", sum(EndPoints), 
            "\n"))
        SeriousProblem <- F
    }
    if (is.null(count)) {
        if (any(MultiplePoints)) 
            stop("Must provide count column if mstart/mend used")
        count <- rep(1, length(time))
    }
    count[SEndPoints | SStartPoints] <- 1
    maxlen <- sum(EndPoints) + length(uniqueID)
    WindowL <- rep(NA, maxlen)
    WindowU <- WindowL
    time.order <- order(time)
    WindowPoint <- rep(NA, length(uniqueID))
    names(WindowPoint) <- uniqueID
    WindowCounts <- WindowPoint
    WindowPoint[1] <- 1
    for (i in 1:length(uniqueID)) {
        WindowPointStart <- WindowPoint[i]
        ProblemLocal <- F
        the.ones <- uniqueID[i] == ID & (StartPoints | EndPoints)
        number.hits <- length(the.ones[the.ones])
        
        if (number.hits > 0) {
            time.order <- order(time[the.ones])
            IndNames <- rep("", length(time.order))
            IndNames[EndPoints[the.ones]] <- "E"
            IndNames[StartPoints[the.ones]] <- "S"
            the.ordered.time <- time[the.ones][time.order]
            the.ordered.counts <- count[the.ones][time.order]
            names(the.ordered.time) <- IndNames
            if (EndPoints[the.ones][1]) {
                the.ordered.time <- c(S = 0, the.ordered.time)
            }
            number.sub.times <- length(the.ordered.time)
            IndNames <- names(the.ordered.time)
            CheckVec <- rep(NA, number.sub.times)
            CheckVec[IndNames == "S"] <- 1
            CheckVec[IndNames == "E"] <- -1
            if (sum(CheckVec) != 0) {
                ProblemLocal <- T
                cat("Different numbers of Start and End\n")
            }
            if (sum(CheckVec[-1] + CheckVec[-number.sub.times]) != 
                0) {
                ProblemLocal <- T
                cat("Start/End indicators are out of order\n")
            }
            if (ProblemLocal) {
                print(the.ordered.time)
                print(CheckVec)
                SeriousProblem <- T
                warning(paste("\nUnit ID", as.character(uniqueID[i]), 
                  "will not be included in the data set"))
            } else {
                NumNewWin <- number.sub.times/2
                WindowL[WindowPointStart:(WindowPointStart + 
                  NumNewWin - 1)] <- the.ordered.time[IndNames == 
                  "S"]
                WindowU[WindowPointStart:(WindowPointStart + 
                  NumNewWin - 1)] <- the.ordered.time[IndNames == 
                  "E"]
                WindowCounts[WindowPointStart:(WindowPointStart + 
                  NumNewWin - 1)] <- the.ordered.counts[IndNames == 
                  "S"]
                if (i < length(uniqueID)) 
                  WindowPoint[i + 1] <- WindowPointStart + NumNewWin
                else LastWindowIndex <- WindowPointStart + NumNewWin - 
                  1
            }
        } else {
            cat("\nRow=", i, "TestWindow  Unit ID", as.character(uniqueID[i]), 
                "WindowPointStart=", WindowPointStart, "\n")
            warning(paste("\nNo start or end indicators for ID line", 
                i, as.character(uniqueID[i])))
            WindowPoint[i + 1] <- WindowPointStart
            LastWindowIndex <- WindowPointStart + NumNewWin - 
                1
            SeriousProblem <- T
        }
    }
    length(WindowL) <- LastWindowIndex
    length(WindowU) <- LastWindowIndex
    if (map.SMRDDebugLevel() > 0) {
        print(WindowL)
        print(WindowU)
        print(WindowPoint)
        print(WindowCounts)
    }
    return.list <- list(IDOrdered = uniqueID, IDOrigOrder = uniqueIDorig, 
        WindowL = WindowL, WindowU = WindowU, WindowPoint = WindowPoint, 
        WindowCounts = WindowCounts, SeriousProblem = SeriousProblem)
    invisible(return.list)
}
