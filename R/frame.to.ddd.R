frame.to.ddd <-
function (frame, 
          response.column, 
          time.column, 
          censor.column,
          case.weight.column, 
          failure.mode.column, 
          right.censor.names = GetSMRDDefault("SMRD.RcName"),
          left.censor.names = GetSMRDDefault("SMRD.LcName"), interval.censor.names = GetSMRDDefault("SMRD.IcName"),
          sinterval.censor.names = GetSMRDDefault("SMRD.DefaultSintervalCensorNames"),
          failure.censor.names = GetSMRDDefault("SMRD.FailName"),
          data.title = deparse(substitute(the.frame)), time.units = NULL,
          response.units = NULL, 
          x.columns = NULL, 
          xlabel = NULL, 
          data.note = "",
          func.call = match.call(), 
          file.name, 
          skip = 0)
{
    if (missing(frame)) {
        if (missing(file.name))
            stop("Must provide either frame or file.name")
        frame <- read.table(file.name, header = T, skip = skip)
        the.mode <- "data.frame"
        dynamic.data.object <- F
        if (is.null(data.title))
            data.title <- file.name
  } else {
        if (!missing(file.name))
            stop("Cannot provide both frame and file.name")
        if (is.character(frame)) {
            the.mode <- "character"
            frame.name <- frame
            if (is.null(data.title))
                data.title <- frame.name
            dynamic.data.object <- T
            frame <- get(envir = .frame0, frame.name)
            
      } else {
        
            the.mode <- "data.frame"
            if (is.null(data.title))
                data.title <- deparse(substitute(frame))
            dynamic.data.object <- F
            
            if (!is.data.frame(frame))
                stop("Need to input either a frame or a file.name that can be read into a data frame")
            
        }
    }
    names.the.frame <- names(frame)
    names(names.the.frame) <- names.the.frame
    ncol.data.mat <- ncol(as.matrix(frame))
    
    if (is.null(time.units)) {
        time.units <- names.the.frame[time.column]
        names(time.units) <- NULL
    }
    
    response.column <- check.column(response.column, 
                                    ncol.data.mat,
                                    names.the.frame, 
                                    number.col.allowed = c(1, 2))
    
    if (is.null(response.units)) {
        response.units <- names.the.frame[response.column[1]]
        names(response.units) <- NULL
    }
    
    time.column <- check.column(time.column, 
                                ncol.data.mat, 
                                names.the.frame)
    
    if (missing(censor.column) || is.null(censor.column)) {
        censor.column <- NULL
        
  } else {
    
        censor.column <- check.column(censor.column, 
                                      ncol.data.mat,
                                      names.the.frame)
  }
    
    if (!is.null(x.columns)) {
        x.columns <- check.column(x.columns, 
                                  ncol.data.mat, 
                                  names.the.frame,
                                  number.col.allowed = -1)
    }
    
    if (missing(case.weight.column) || is.null(case.weight.column)) {
      
        case.weight.column <- NULL
  
        } else {
          
        case.weight.column <- check.column(case.weight.column,
            ncol.data.mat, names.the.frame)
        }
    
    if (missing(failure.mode.column) || is.null(failure.mode.column)) {
      
        failure.mode.column <- NULL
  
        } else {
          
        failure.mode.column <- check.column(failure.mode.column,
                                            ncol.data.mat, 
                                            names.the.frame)
        }
    
    `if`(dynamic.data.object,
         rlist <- frame.name,
         rlist <- frame)
    
    attr(rlist, "right.censor.names") <- right.censor.names
    attr(rlist, "left.censor.names") <- left.censor.names
    attr(rlist, "interval.censor.names") <- interval.censor.names
    attr(rlist, "failure.censor.names") <- failure.censor.names
    attr(rlist, "response.column") <- response.column
    attr(rlist, "censor.column") <- censor.column
    attr(rlist, "time.column") <- time.column
    attr(rlist, "case.weight.column") <- case.weight.column
    attr(rlist, "failure.mode.column") <- failure.mode.column
    attr(rlist, "data.title") <- data.title
    attr(rlist, "time.units") <- time.units
    attr(rlist, "response.units") <- response.units
    attr(rlist, "x.columns") <- x.columns
    attr(rlist, "xlabel") <- xlabel
    attr(rlist, "data.note") <- data.note
    attr(rlist, "func.call") <- func.call
    attr(rlist, "date.made") <- date()
    oldClass(rlist) <- c("Dest.Degrad.data", "data.frame")
    MysetOldClass(attr(rlist, "class"))
    return(Fix.AsIs(rlist))
}
