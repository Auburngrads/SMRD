#' Title
#'
#' @param data.rdu 
#' @param debug1 
#' @param kprint
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' #time.column, ID.column, cost.count.column, event.column
#' WorkStation.rdu <- frame.to.rdu(workstation,
#'                                 ID.column = "station",
#'                                 time.column = "days",
#'                                 event.column = "event")
#' 
#' #attr(WorkStation.rdu,"WindowInfo")
#' 
#' WorkStation.mcf <- mcf(WorkStation.rdu)
#' 
#' #sqrt(WorkStation.mcf$Var)
#' 
#' event.plot(WorkStation.rdu)
#' 
#' plot(WorkStation.mcf)
#' 
#' }
mcf <-
function (data.rdu,
          debug1 = F,
          kprint = 0) 
{
    event <- events(data.rdu)
    EndPoints <- is.element(casefold(event), c("end", "mend", "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(data.rdu)
    UnitID <- as.factor(get.UnitID(data.rdu))
    WindowInfo <- attr(data.rdu, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    RecurrTimes <- Times[CriticalEvent, ]
    RecurrUnitID <- UnitID[CriticalEvent]
    RecurrCosts <- get.Costs(data.rdu)[CriticalEvent]
    numRecurr <- length(RecurrTimes)
    if (debug1) browser()
    
    mcfdata.out <- XXMCF(numrecurr = as.integer(numRecurr),
                         timeofrecurr = as.double(RecurrTimes), 
                         krecurrid = as.integer(RecurrUnitID), 
                         dcost = as.double(RecurrCosts), 
                         muniqrecurr = integer(1), 
                         tuniq = double(numRecurr), 
                         apoint = integer(numRecurr), 
                         lnumrecurr = integer(1), 
                         delta = integer(numRecurr), 
                         nunitsgroups = as.integer(length(WindowInfo$WindowPoint)), 
                         wpoint = as.integer(WindowInfo$WindowPoint), 
                         nwindows = as.integer(length(WindowInfo$WindowL)), 
                         twindowsl = as.double(WindowInfo$WindowL), 
                         twindowsu = as.double(WindowInfo$WindowU), 
                         wcounts = as.integer(WindowInfo$WindowCounts), 
                         inwindowj = integer(length(WindowInfo$WindowPoint)),
                         muhat = double(numRecurr), 
                         varhat = double(numRecurr), 
                         dbar = double(numRecurr), 
                         iordl = integer(length(WindowInfo$WindowL)), 
                         iordu = integer(length(WindowInfo$WindowL)), 
                         iorder = integer(numRecurr), 
                         iscrat = integer(numRecurr),
                         kdebug = as.integer(kprint))
    
    max.censoring.time <- max(WindowInfo$WindowU)
    muniqrecurr <- mcfdata.out$muniqrecurr
    varhat <- mcfdata.out$varhat[1:muniqrecurr]
    missing.or.na <- is.na(varhat) | varhat < 0
    
    if (any(missing.or.na)) {
        
        warning("Negative or missing variances---setting equal to 0")
        if (map.SMRDDebugLevel() >= 4) print(varhat)
        varhat[missing.or.na] <- 0
        
    }
    the.list <- list(tuniq = mcfdata.out$tuniq[1:muniqrecurr], 
                     muHat = mcfdata.out$muhat[1:muniqrecurr],
                     VarHat = varhat, 
                     max.censoring.time = max.censoring.time,
                     title = get.data.title(data.rdu), 
                     time.units = get.time.units(data.rdu))
    
    if (debug1) browser()
    oldClass(the.list) <- "mcf"
    return(the.list)
}
