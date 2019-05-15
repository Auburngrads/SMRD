#' Title
#'
#' @param data.rdu 
#' @param theta.mat 
#' @param form 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AMSAAExactFail.rdu <- frame.to.rdu(amsaaexactfail,
#'                                    ID.column = "vehicle", 
#'                                    time.column = "miles" , 
#'                                    event.column = "event")
#' 
#' theta.mat <- matrix(c(1,1,2,2),2,2)
#' theta.mat <- c(1,2)
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "power.law")
#' 
#' theta.mat <- matrix(c(.01,.01,.02,.02),2,2)
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "log.linear")
#' 
#' }
loglikeNHPPvec <-
function (data.rdu, theta.mat, form)
{
    if (!is.matrix(theta.mat))
        theta.mat <- as.matrix(theta.mat)
    time.column <- attr(data.rdu, "time.column")
    event.column <- attr(data.rdu, "event.column")
    WindowInfo <- attr(data.rdu, "WindowInfo")
    event <- data.rdu[, event.column]
    Times <- data.rdu[, time.column]
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    Cevent <- !(EndPoints | StartPoints)
    Cevent.times <- Times[Cevent]
    
    zout <- SLOGLIKENHPP(time = as.double(Cevent.times),
                         ntimes = as.integer(length(Cevent.times)), 
                         timel = as.double(WindowInfo$WindowL),
                         timeu = as.double(WindowInfo$WindowU), 
                         kwcount = as.integer(WindowInfo$WindowCounts),
                         nwindows = as.integer(length(WindowInfo$WindowU)), 
                         kform = as.integer(num.nhpp.form(generic.nhpp.form(form)[[1]])),
                         thetav = as.double(t(theta.mat)), 
                         nparm = as.integer(nrow(theta.mat)),
                         ntheta = as.integer(ncol(theta.mat)), 
                         answer = double(ncol(theta.mat)))
    
    return(zout$answer)
}
