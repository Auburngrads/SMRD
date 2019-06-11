#' Title
#'
#' @param data.rdu 
#' @param my.title
#' @param which
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
function (data.rdu, 
          my.title = NULL, 
          which = NULL) 
{
    
    if(!is.null(which) && (which < 0 || which > 3)){
        
        stop("which must be NULL or an integer in [1,3]")
        
    }
    
    if(is.null(which) || which == 1) {
        
       repair.tsplot(data.rdu, my.title = my.title)
       if(is.null(which)) pause()
    
    }
    
    if(is.null(which) || which == 2) {
        
       interarrival.plot(data.rdu, my.title = my.title)
       if(is.null(which)) pause()
    
    }
    
    if(is.null(which) || which == 3) {
        
       ar1.plot(data.rdu, my.title = my.title)
        
    }
    
    invisible()
    
}
