#' Title
#'
#' @param data.ld 
#' @param allow 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ZelenCap.ld <- frame.to.ld(zelencap,
#'                            response.column = 1,
#'                            censor.column = 2,
#'                            case.weight.column = 3,
#'                            x.columns = c(4, 5),
#'                            time.units = "Hours")
#' 
#' xmat(ZelenCap.ld)
#' 
#' }
xmat <- function (data.ld, allow = T)
{
    frame.type <- data.object.type(data.ld)
    switch(frame.type[1], frame.centered = {
        if (is.character(data.ld) && length(data.ld == 1)) data.ld <- get(envir = .frame0, data.ld)

        x.columns <- attr(data.ld, "x.columns")

        if (is.null(x.columns)) {

          if (allow) the.xmat<-NULL  else stop("Null xmat")

        } else {

          x.columns<-as.character(attr(data.ld, "x.columns"))
          data.ld<-as.data.frame(data.ld)
          the.xmat <- data.ld[c(which(colnames(data.ld)%in%x.columns))]
          oldClass(the.xmat) <- "data.frame"

        }

    }, list.centered = {
        x.columns <- data.ld$x.columns
        if (!is.null(x.columns)) {
            the.xmat <- data.ld$frame[x.columns]
            oldClass(the.xmat) <- "data.frame"
        } else the.xmat <- NULL
    }, unfolded = {
        return(data.ld$xmat)
    }, {
        stop("Corrupted data frame")
    })
    if (!allow && is.null(the.xmat))
        stop("Null xmat")

    return(the.xmat)
}

#
#

`xmat<-` <-
  function (data.ld, value = NULL) 
  {
    frame.type <- data.object.type(data.ld)
    switch(frame.type[1], frame.centered = {
      if (is.null(value)) {
        attr(data.ld, "x.columns") <- NULL
        return(data.ld)
      }
      x.columns <- attr(data.ld, "x.columns")
      all.attributes <- attributes(data.ld)
      oldClass(data.ld) <- "data.frame"
      frame.names <- names(data.ld)
      x.col.numbers <- match(names(data.ld[, x.columns, drop = F]), 
                             frame.names)
      new.xmat.names <- make.names(colnames(value))
      dimnames(value) <- list(as.character(1:nrow(value)), 
                              new.xmat.names)
      old.part.frame <- data.ld[, -x.col.numbers, drop = F]
      new.frame.names <- c(names(old.part.frame), colnames(value))
      new.frame <- data.frame(old.part.frame, value)
      names(new.frame) <- new.frame.names
      all.attributes$x.columns <- colnames(value)
      all.attributes$names <- names(new.frame)
      attributes(new.frame) <- all.attributes
      if (map.SMRDDebugLevel() >= 6) cix(new.frame)
      return(new.frame)
    }, , unfolded = {
      data.ld$xmat <- value
      data.ld$x.columns <- colnames(value)
      return(data.ld)
    })
  }
