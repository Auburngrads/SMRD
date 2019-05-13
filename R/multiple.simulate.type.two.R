#' Title
#'
#' @param n 
#' @param r 
#' @param distribution 
#' @param number.sim 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' mstt1 <- multiple.simulate.type.two(n = c(20,30,40), 
#'                                     r = c(20,30,40),
#'                                     distribution = "lognormal",
#'                                     number.sim = 1000)
#' 
#' plot(mstt1, qprob = 0.9)
#' plot(mstt1, qprob = 0.8)
#' plot(mstt1, qprob = 0.5)
#' 
#' 
#' mstt2 <- multiple.simulate.type.two(n = c(20,30,40), 
#'                                     r = c(10,15,20),
#'                                     distribution = "lognormal",
#'                                     number.sim = 1000)
#' 
#' plot(mstt2, qprob = 0.9)
#' plot(mstt2, qprob = 0.8)
#' plot(mstt2, qprob = 0.5, grids = T)
#' 
#' mstt3 <- multiple.simulate.type.two(n = c(24,28,33), 
#'                                     r = c(24,28,33),
#'                                     distribution = "normal", 
#'                                     number.sim = 1000)
#' 
#' plot(mstt3, qprob = 0.9)
#' }
multiple.simulate.type.two <-
function (n, r, distribution, number.sim = 1000)
{
    if (length(n) != length(r))
        stop(paste("Length of n=", length(n), "and r=", length(r),
            "need to be the same"))
    n.lt.2 <- n < 2
    if (any(n.lt.2))
        stop(paste("Values of n must be greater than 1",
            n[n.lt.2]))
    r.lt.2 <- r < 2
    if (any(r.lt.2))
        stop(paste("Values of r must be greater than 1",
            r[r.lt.2]))
    r.gt.n <- r > n
    if (any(r.gt.n))
        stop(paste("Values of r", r[r.gt.n], "greater than n",
            n[r.gt.n]))
    results <- list()
    for (i in 1:length(n)) {
        result.name <- paste("n=", n[i], "r=", r[i], sep = "")
        results[[result.name]] <- sim.type.two(n = n[i],
            r = r[i], distribution = distribution, number.sim = number.sim)
    }
    attr(results, "date") <- date()
    oldClass(results) <- "prob.succ.demo.type2"
    return(results)
}
