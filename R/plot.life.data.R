#' @export
plot.life.data <-
function (x, distribution = NULL, ...)
{
    if (!is.null(distribution)) {
      
      if (length(distribution)==4) {
        tmp <- four.npprobplot(data.ld = x, distribution.list = distribution,...)
      }
      
      if (length(distribution)==2) {
        tmp <- two.npprobplot(data.ld = x, distribution.list = distribution,...)
      }
      
      if (length(distribution)==1) {
        tmp <- npprobplot(data.ld = x, distribution = distribution,...)
      }
      
      if (length(distribution)==3 | length(distribution) > 4) {
        stop('The number of distributions must be either 1, 2, or 4')
      }
      
  
    } else { tmp <- cdfplot(data.ld = x, ...)  }
  
    invisible(tmp)
}
