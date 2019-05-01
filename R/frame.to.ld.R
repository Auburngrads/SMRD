#' Create a \code{life.data} object from a \code{data.frame} 
#'
#' @param frame  A \code{data.frame} class object.
#' @param response.column The numeric index or name (as a character string) of the column(s) in \code{frame} containing the responses.  
#'    
#'    For reliability data, responses are often the amount of usage (measured in time, distance, cycles) that are recorded when an event occurred.
#'    
#'    Events for which responses should be recorded include when one or more units fail or are censored. If the responses are recorded as an interval (i.e. an event occurs between a and b), \code{response.column} will accept vector arguments. 
#'    
#' @param censor.column The numeric index or name (as a character string) of the column in \code{frame} that contains the type(s) of censoring (exact failures, right-censoring, left-censoring, interval censoring) occurring at each event.
#'    
#'    In practice, several different labels are used to refer to differnt types of censoring.  \code{frame.to.ld} can accept many of these different labels.  To view the list of see \code{SMRDOptionsDefaults}.
#'    
#' @param case.weight.column The numeric index or name (as a character string) of the column in \code{frame} containing the number of events occurring at each entry in the \code{response.column}.
#'    \bold{(optional)} This column may be left undefined, in which case it will be assumed that only one unit was observed to have failed/censored for each entry in \code{response.column}.
#'    
#' @param failure.mode.column  The numeric index or name (as a character string) of the column in \code{frame} containing the type(s) of failure modes observed.  
#'    \bold{(optional)} This column may be left undefined, in which case it will be assumed that only one failure mode was observed
#'
#' @param truncation.type.column
#' 
#' @param truncation.response.column The time at which an obsevation is trucated.
#' 
#' @param time.units A character string denoting the unit of measure used to quantify system lifetime.
#'    \code{time.units} should be provided for each \code{life.data} object as this string will be automatically substituted in many SMRD plots.
#'
#' @param x.columns
#' 
#' @param xlabel
#' 
#' @param data.note
#' 
#' @param func.call
#' 
#' @param residual.rmd
#' 
#' @author William Q. Meeker, PhD
#' 
#' @seealso \code{\link{frame.to.rmd}}, \code{\link{frame.to.ddd}}, \code{\link{surv.to.ld}}, \code{\link{SMRDOptionsDefaults}}
#' 
#' @examples
#' \dontrun{
#' # lzbearing data set
#' lzbearing.ld <- frame.to.ld(lzbearing, 
#'                             response.column = 1,
#'                             time.units = "Megacycles")
#' }
#' @export  

frame.to.ld <-
function (frame,
          response.column,
          censor.column,
          case.weight.column,
          failure.mode.column,
          truncation.type.column,
          truncation.response.column,
          time.units = names.the.frame[response.column[1]],
          x.columns = NULL,
          xlabel = NULL,
          data.title = deparse(substitute(frame)),
          data.note = "",
          func.call = match.call(),
          residual.rmd = NULL)
{
  right.censor.names = GetSMRDDefault("SMRD.RcName")
  left.censor.names = GetSMRDDefault("SMRD.LcName")
  interval.censor.names = GetSMRDDefault("SMRD.IcName")
  sinterval.censor.names = GetSMRDDefault("SMRD.DefaultSintervalCensorNames")
  failure.censor.names = GetSMRDDefault("SMRD.FailName")
  
    if (missing(frame)) {
      
        stop("Must provide a data frame")
        the.mode <- "data.frame"
        dynamic.data.object <- F
    
        } else {

        if (is.character(frame)) {
          
            the.mode <- "character"
            frame.name <- frame
            
            if (is.null(data.title)) data.title <- frame.name
            
            dynamic.data.object <- T
    
            } else {
              
            the.mode <- "data.frame"
            dynamic.data.object <- F
            if (!is.data.frame(x = frame))
                stop("Need to input either a frame that can be read into a data frame")
        }
    }
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    
    response.column <- SMRD:::check.column(response.column, 
                                    ncol.data.mat,
                                    names.the.frame, 
                                    number.col.allowed = c(1, 2))
    
    `if`(missing(censor.column) || is.null(censor.column),
         censor.column <- NULL,
         censor.column <- SMRD:::check.column(censor.column, 
                                       ncol.data.mat,
                                       names.the.frame))
    
    `if`(missing(truncation.type.column) || is.null(truncation.type.column),
         truncation.type.column <- NULL,
         truncation.type.column <- SMRD:::check.column(truncation.type.column,
                                                ncol.data.mat, 
                                                names.the.frame))
         
    if (missing(truncation.response.column) || is.null(truncation.response.column)) {
        if (!is.null(truncation.type.column))
            stop("Must specify truncation response if truncations type is specified")
        truncation.response.column <- NULL
        } else {
        truncation.response.column <- SMRD:::check.column(truncation.response.column,
                                                   ncol.data.mat, 
                                                   names.the.frame, 
                                                   number.col.allowed = c(1,2))
        if (is.null(truncation.type.column))
            stop("Must not specify truncation response if truncations type is not specified")
        }

    if (!is.null(x.columns)) {
        x.columns <- SMRD:::check.column(x.columns, 
                                  ncol.data.mat, 
                                  names.the.frame,
                                  number.col.allowed = -1)
        }
    
    `if`(missing(case.weight.column) || is.null(case.weight.column),
         case.weight.column <- NULL,
         case.weight.column <- SMRD:::check.column(case.weight.column,
                                            ncol.data.mat, 
                                            names.the.frame))
    
    `if`(missing(failure.mode.column) || is.null(failure.mode.column),
         failure.mode.column <- NULL,
         failure.mode.column <- SMRD:::check.column(failure.mode.column,
                                             ncol.data.mat, 
                                             names.the.frame))
    
    `if`(dynamic.data.object,
         rframe <- frame.name,
         rframe <- frame)

    attr(rframe, "right.censor.names") <- right.censor.names
    attr(rframe, "left.censor.names") <- left.censor.names
    attr(rframe, "interval.censor.names") <- interval.censor.names
    attr(rframe, "failure.censor.names") <- failure.censor.names
    attr(rframe, "sinterval.censor.names") <- sinterval.censor.names
    attr(rframe, "response.column") <- response.column
    attr(rframe, "censor.column") <- censor.column
    attr(rframe, "case.weight.column") <- case.weight.column
    attr(rframe, "failure.mode.column") <- failure.mode.column
    attr(rframe, "truncation.response.column") <- truncation.response.column
    attr(rframe, "truncation.type.column") <- truncation.type.column
    attr(rframe, "data.title") <- data.title
    attr(rframe, "time.units") <- time.units
    attr(rframe, "x.columns") <- x.columns
    attr(rframe, "xlabel") <- xlabel
    if (is.data.frame(rframe))
        attr(rframe, "names") <- names.the.frame
    attr(rframe, "data.note") <- data.note
    attr(rframe, "residual.rmd") <- residual.rmd
    oldClass(rframe) <- get.life.data.class(rframe)
    SMRD.sanity(x = Fix.AsIs(rframe))
    MysetOldClass(attr(rframe, "class"))
    return(rframe)
}
