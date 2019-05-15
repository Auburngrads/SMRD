#' Title
#'
#' @param data.rdu 
#' @param my.title 
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
#' 
#' renewal.plots(halfbeak.rdu)
#' 
#' }
renewal.plots <-
function (data.rdu, my.title = NULL) 
{
    repair.tsplot(data.rdu)
    pause()
    interarrival.plot(data.rdu)
    pause()
    ar1.plot(data.rdu)
    invisible()
}
