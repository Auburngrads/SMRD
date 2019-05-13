#' @export
print.repeated.measures.data <-
function (x, quote = F, prefix = "",...)
{
    cat(paste("\n", get.data.title(x), "repeated measured data\n\n"))
    Unit.marker <- attr(x, "Unit.marker")
    response.column <- attr(x, "response.column")
    time.column <- attr(x, "time.column")
    time.units <- attr(x, "time.units")
    response.units <- get.response.units(x)
    the.x.columns <- get.x.columns(x)
    the.print.frame <- data.frame(Unit.marker, x[[time.column]],
        x[[response.column]])
    dimnames(the.print.frame)[[2]] <- c("Unit", time.units, response.units)
    
    if (!is.null(get.x.columns(x))) {
      
        the.print.frame <- data.frame(the.print.frame, 
                                      x[,the.x.columns])
        
        dimnames(the.print.frame)[[2]] <- c("Unit", 
                                            time.units,
                                            response.units, 
                                            get.xlabel(x))
    }
    print.data.frame(the.print.frame)
}
