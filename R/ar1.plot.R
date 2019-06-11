#' Title
#'
#' @importFrom stats acf
#'
#' @param data.rdu 
#' @param my.title 
#' @param xlab 
#' @param ylab 
#' @param nacf 
#' @param plot.acf 
#' @param ... 
#'
#' @return NULL
#' @export
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
#' ar1.plot(halfbeak.rdu,
#'          xlab = "Lagged Thousands of Hours Between Maintenance Actions",
#'          ylab = "Thousands of Hours Between Maintenance Actions")
#' 
#' ar1.plot(halfbeak.rdu,
#'          xlab = "Lagged Thousands of Hours Between Maintenance Actions",
#'          ylab = "Thousands of Hours Between Maintenance Actions",
#'          plot.acf = T)
#' 
#' 
#' 
#' }
ar1.plot <-
function (data.rdu, 
          my.title = NULL, 
          xlab = "Interarrival Times", 
          ylab = "Lagged Interarrival Times", 
          nacf = 5, 
          plot.acf = F,...) 
{
    old.par <- par(mar = c(5.1, 6.1, 4.1, 2.1), pty = "s")
    
    on.exit({
        par(old.par)
        par(new = F)
    })
    
    the.interarrival.times <-Response(interarrival.times(data.rdu))
    number.failures <- length(the.interarrival.times)
    plot(the.interarrival.times[-1], 
         the.interarrival.times[-number.failures],
         xlab = "", 
         ylab = "", 
         cex = 1.5, 
         las = 1, ...)
    
    title(xlab = xlab, cex = 1.5)
    title(ylab = ylab, cex = 1.5, mgp = c(4, 1, 0))
    
    if (is.null(my.title)) my.title <- get.data.title(data.rdu)
    title(my.title)
    
    acf.values <- stats::acf(the.interarrival.times, plot = plot.acf)$acf[1:nacf]
    acf.tvalues <- acf.values * sqrt((length(the.interarrival.times) - 0:(nacf - 1)))
    
    return(list(acf.values = acf.values[-1], acf.tvalues = acf.tvalues[-1]))
}
