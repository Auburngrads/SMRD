#' Title
#'
#' @param frame 
#' @param time.column 
#' @param ID.column 
#' @param cost.count.column 
#' @param event.column 
#' @param time.units 
#' @param failure.censor.names 
#' @param right.censor.names 
#' @param left.censor.names 
#' @param interval.censor.names 
#' @param data.title 
#' @param data.note 
#' @param skip 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
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
#' }
frame.to.rdu <-
function (frame, 
          time.column, 
          ID.column, 
          cost.count.column, 
          event.column,
          time.units = names.the.frame[time.column[1]], 
          failure.censor.names = GetSMRDDefault("SMRD.FailName"),
          right.censor.names = GetSMRDDefault("SMRD.RcName"), 
          left.censor.names = GetSMRDDefault("SMRD.LcName"),
          interval.censor.names = GetSMRDDefault("SMRD.IcName"),
          data.title, 
          data.note = "", 
          skip = 0)
{
    if (missing(data.title)) data.title = deparse(substitute(frame))
    
    if (is.character(frame)) {
      
        frame <- read.table(frame, header = T, skip = skip)
        if (is.null(data.title)) data.title <- file
        
    } else {
      
        if (!is.data.frame(frame)) stop("Need to input either a frame or a file that can be read into a data frame")
      
    }
    
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(frame)
    
    `if`(missing(time.column),
         stop("Must specify Time column"),
         time.column <- check.column(time.column, ncol.data.mat,names.the.frame))
    
    if (missing(ID.column)) {
      
        stop("Must specify unit ID column")
      
    } else {
      
        ID.column <- check.column(ID.column, ncol.data.mat, names.the.frame)
        frame[[ID.column]] <- as.factor(frame[[ID.column]])
        
    }
    
    `if`(missing(event.column),
         stop("Must specify Events column"),
         event.column <- check.column(event.column, 
                                      ncol.data.mat,
                                      names.the.frame))
    
    `if`(missing(cost.count.column),
         cost.count.column <- NULL,
         cost.count.column <- check.column(cost.count.column,
                                           ncol.data.mat, 
                                           names.the.frame))
    
    attr(frame, "data.title") <- data.title
    attr(frame, "time.units") <- time.units
    attr(frame, "time.column") <- time.column
    attr(frame, "ID.column") <- ID.column
    attr(frame, "event.column") <- event.column
    attr(frame, "cost.count.column") <- cost.count.column
    attr(frame, "right.censor.names") <- right.censor.names
    attr(frame, "left.censor.names") <- left.censor.names
    attr(frame, "interval.censor.names") <- interval.censor.names
    attr(frame, "failure.censor.names") <- failure.censor.names
    attr(frame, "data.note") <- data.note
    attr(frame, "date.made") <- date()
    
    `if`(is.null(cost.count.column),
         WindowInfo <- TestWindow(frame[, ID.column], 
                                  frame[, time.column], 
                                  frame[, event.column],
                                  rep(1, nrow(frame))),
         WindowInfo <- TestWindow(frame[, ID.column], 
                                  frame[, time.column], 
                                  frame[, event.column], 
                                  frame[, cost.count.column]))
    
    attr(frame, "WindowInfo") <- WindowInfo
    oldClass(frame) <- c("recurrence.data", "data.frame")
    MysetOldClass(attr(frame, "class"))
    frame <- SMRD.sanity(frame)
    return(frame)
    
}
