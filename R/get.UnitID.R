#' Title
#'
#' @param data.d 
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
#' 
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "power.law")
#' 
#' TestLike(AMSAAExactFail.rdu, theta.mat, "power.law")
#' 
#' theta.mat <- matrix(c(.01,.01,.02,.02),2,2)
#' loglikeNHPPvec(AMSAAExactFail.rdu, theta.mat, "log.linear")
#' 
#' attr(AMSAAExactFail.rdu,"WindowInfo")
#' 
#' RiskSet(AMSAAExactFail.rdu)
#' 
#' plotRiskSet(AMSAAExactFail.rdu,proportion = T)
#' 
#' get.UnitID(AMSAAExactFail.rdu)
#' 
#' mcf(AMSAAExactFail.rdu)
#' 
#' 
#' }
get.UnitID <-
function (data.d) 
{
    UnitID.column <- get.UnitID.column(data.d)
    if (is.null(UnitID.column)) 
        stop("Internal error SMRD---cannot find UnitIDs")
    the.UnitIDs <- data.d[, UnitID.column]
    return(the.UnitIDs)
}
