#' Title
#'
#' @param frame 
#' @param levels.columns 
#' @param censor.column 
#' @param allocation.column 
#' @param describe.string 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AdhesiveBond2.frame <- data.frame(DegreesC = c(80,100,120),
#'                                   SampleSize = c(150,60,90),
#'                                   CensorTimes = c(243,243,243))
#' 
#' AdhesiveBond2.altplan <- 
#'   get.alt.test.plan.frame(AdhesiveBond2.frame,
#'                           levels.columns = "DegreesC", 
#'                           censor.column = "CensorTimes",
#'                           allocation.column = "SampleSize",
#'                           describe.string = "AdhesiveBond Test Plan")
#' 
#' print(AdhesiveBond2.altplan)
#' 
#' }
get.alt.test.plan.frame <-
function (frame, 
          levels.columns, 
          censor.column, 
          allocation.column,
          describe.string = "")
{
    if (!is.data.frame(frame)) frame <- get(envir = .frame0, frame)
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    
    if (missing(censor.column) || is.null(censor.column)) {
      
        censor.times <- NULL
        
  } else {
    
        censor.column <- check.column(censor.column, 
                                      ncol.data.mat,
                                      names.the.frame)
        
        censor.times <- frame[[censor.column]]
  }
    
    allocation.column <- check.column(allocation.column, 
                                      ncol.data.mat,
                                      names.the.frame)
    
    levels.columns <- check.column(levels.columns, 
                                   ncol.data.mat,
                                   names.the.frame, 
                                   number.col.allowed = -1)
    
    the.levels <- frame[, levels.columns, drop = F]
    
    return(get.alt.test.plan.direct(accel.variable.levels = the.levels,
                                    number.of.units = frame[[allocation.column]], 
                                    censor.times = censor.times,
                                    accelvar.names = names(the.levels), 
                                    describe.string = describe.string))
}
