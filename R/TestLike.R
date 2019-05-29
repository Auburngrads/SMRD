#' Title
#'
#' @param data.rdu 
#' @param theta 
#' @param form 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' TestWindow(amsaaexactfail[[1]],
#'            amsaaexactfail[[2]],
#'            amsaaexactfail[[3]],
#'            NULL)
#' 
#' AMSAAExactFail.rdu <- frame.to.rdu(amsaaexactfail,
#'                                    ID.column = "vehicle", 
#'                                    time.column = "miles" , 
#'                                    event.column = "event")
#' 
#' names(attributes(AMSAAExactFail.rdu))
#' 
#' # testing the loglikelihood
#' theta.mat <- matrix(c(1,1,2,2),2,2)
#' #theta.mat <- c(1,2)
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "power.law")
#' 
#' TestLike(AMSAAExactFail.rdu, theta.mat, "power.law")
#' 
#' theta.mat <- matrix(c(.01,.01,.02,.02),2,2)
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "log.linear")
#' 
#' 
#' }
TestLike <-
function (data.rdu, theta, form) 
{
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    WindowInfo <- attr(data.rdu, "WindowInfo")
    event <- data.rdu[, event.column]
    Times <- data.rdu[, time.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    Cevent <- !(EndPoints | StartPoints)
    Cevent.times <- Times[Cevent]
    RecurrCosts <- get.Costs(data.rdu)[Cevent]
    Cevent.counts <- Times[Cevent]
    answer <- Sxloglikenhpp(Cevent.times, RecurrCosts, WindowInfo$WindowL, 
        WindowInfo$WindowU, WindowInfo$WindowCounts, form, theta)
    return(answer)
}
