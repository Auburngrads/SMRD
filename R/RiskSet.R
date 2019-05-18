#' Title
#'
#' @param data.rdu 
#' @param kdebug1 
#' @param JustEvent 
#'
#' @return NULL
#' @export
#' @name RiskSet
#' @rdname RiskSet_r
#'
#' @examples
#' \dontrun{
#' 
#' halfbeak.rdu <- frame.to.rdu(halfbeak,
#'                              ID.column = "unit", 
#'                              time.column = "hours" ,
#'                              event.column = "event", 
#'                              data.title = "Halfbeak Data", 
#'                              time.units = "Thousands of Hours of Operation")
#' 
#' RiskSet(halfbeak.rdu)
#' 
#' }
RiskSet <-
function (data.rdu, 
          kdebug1 = F, 
          JustEvent = T) 
{
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    WindowInfo <- attr(data.rdu, "WindowInfo")
    event <- data.rdu[, event.column]
    Times <- data.rdu[, time.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    Cevent <- !(EndPoints | StartPoints)
    
    `if`(JustEvent,
         tuniq <- unique(sort(Times[Cevent])),
         tuniq <- unique(sort(c(0, Times[Cevent], WindowInfo$WindowL, WindowInfo$WindowU))))
    
    zout <- RISKSET(muniqrecurr = as.integer(length(tuniq)),
                    tuniq = as.double(tuniq), 
                    nwindows = as.integer(length(WindowInfo$WindowU)),
                    twindowsl = as.double(WindowInfo$WindowL), 
                    twindowsu = as.double(WindowInfo$WindowU),
                    wcounts = as.integer(WindowInfo$WindowCounts), 
                    iordl = integer(length(WindowInfo$WindowL)),
                    iordu = integer(length(WindowInfo$WindowL)), 
                    delta = integer(length(tuniq)),
                    kdebug = as.integer(kdebug1), 
                    iscrat = integer(length(WindowInfo$WindowL)))
    
    return(list(Times = zout$tuniq, 
                Counts = zout$delta, 
                NumberUnits = length(unique(get.UnitID(data.rdu)))))
}
