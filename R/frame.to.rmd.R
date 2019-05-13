#' Title
#'
#' @param frame 
#' @param response.column 
#' @param time.column 
#' @param unit.column 
#' @param group.column 
#' @param response.units 
#' @param unit.name 
#' @param data.title 
#' @param time.units 
#' @param x.columns 
#' @param xlabel 
#' @param data.note 
#' @param skip 
#'
#' @return NULL 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' GaAsLaser.rmd <- 
#'   frame.to.rmd(gaaslaser, 
#'                response.column = 1, 
#'                unit.column = 2, 
#'                time.column = 3, 
#'                response.units = "Increase in Operating Current (%)")
#' 
#' summary(GaAsLaser.rmd)
#' plot(GaAsLaser.rmd)
#' trellis.plot(GaAsLaser.rmd, order.groups = T)
#' 
#' trellis.plot(GaAsLaser.rmd, order.groups = F)
#' 
#' # Convert to a \code{life.data} object
#' 
#' GaAsLaser.ld <- rmd.to.ld(GaAsLaser.rmd, 
#'                           fail.level = 10, 
#'                           x.axis = "sqrt")
#' 
#' plot.rmd.residual(GaAsLaser.ld)
#' 
#' GaAsLaser.ld <- rmd.to.ld(GaAsLaser.rmd,
#'                           fail.level = 10)
#' 
#' plot.rmd.residual(GaAsLaser.ld)
#' 
#' summary(GaAsLaser.ld)
#' 
#' }
frame.to.rmd <-
function (frame, 
          response.column, 
          time.column, 
          unit.column, 
          group.column,
          response.units = NULL, 
          unit.name = NULL, 
          data.title = NULL,
          time.units = NULL, 
          x.columns = NULL, 
          xlabel = NULL, 
          data.note = "",
          skip = 0)
{
    if (is.null(frame)) return(NULL)
  
    if(is.null(data.title)) data.title <- deparse(substitute(frame))
  
    if (is.character(frame)) {
      
        `if`(exists("frame"),
            frame <- read.table(frame, header = T, skip = skip),
            stop(paste("File", frame, "does not exist")))
      
  } else {
    
        if (!is.data.frame(frame))
            stop("Input either a frame or a file that can be read as a data frame")
  }
    names.the.frame <- names(frame)
    
    names(names.the.frame) <- names.the.frame
    
    if (is.null(unit.name)) {
        unit.name <- names.the.frame[unit.column]
        names(unit.name) <- NULL
    }
    if (is.null(time.units)) {
        time.units <- names.the.frame[time.column]
        names(time.units) <- NULL
    }
    if (is.null(response.units)) {
        response.units <- names.the.frame[response.column]
        names(response.units) <- NULL
    }
    ncol.data.mat <- ncol(frame)
    response.column <- check.column(response.column, 
                                    ncol.data.mat,
                                    names.the.frame)
    time.column <- check.column(time.column, ncol.data.mat, names.the.frame)
    unit.column <- check.column(unit.column, 
                                ncol.data.mat, 
                                names.the.frame,
                                number.col.allowed = -1)
    
    Unit.marker <- apply(X = as.matrix(as.character(frame[, unit.column])),
                         MARGIN = 1, 
                         FUN = paste, collapse = ".")
    
    `if`(missing(group.column) || is.null(group.column),
         group.column <- NULL,
         group.column <- check.column(group.column, 
                                      ncol.data.mat,
                                      names.the.frame))
    
    if (!is.null(x.columns)) {
        x.columns <- check.column(x.columns, 
                                  ncol.data.mat, 
                                  names.the.frame,
                                  number.col.allowed = -1)
        if (is.null(xlabel)) {
            xlabel <- names.the.frame[x.columns]
            names(xlabel) <- NULL
        }
        
        if (length(xlabel) != length(x.columns))
            stop(paste("xlabel=", paste(xlabel, collapse = ", "),
                "has length different than number of cols in xmat=",
                length(x.columns)))
        
  } else {
    
        xlabel <- NULL
  }
    
    attr(frame, "Unit.marker") <- Unit.marker
    attr(frame, "unit.column") <- unit.column
    attr(frame, "response.column") <- response.column
    attr(frame, "time.column") <- time.column
    attr(frame, "group.column") <- group.column
    attr(frame, "data.title") <- data.title
    attr(frame, "response.units") <- response.units
    attr(frame, "time.units") <- time.units
    attr(frame, "unit.name") <- unit.name
    attr(frame, "x.columns") <- x.columns
    attr(frame, "xlabel") <- xlabel
    attr(frame, "data.note") <- data.note
    oldClass(frame) <- c("repeated.measures.data", "data.frame")
    MysetOldClass(attr(frame, "class"))
    return(frame)
}
