#' Title
#'
#' @param distribution 
#' @param xlab 
#' @param xlim 
#' @param ylim 
#' @param original.par 
#' @param my.title 
#' @param cex 
#' @param cex.labs 
#' @param cex.tic.lab 
#' @param cex.title 
#' @param sub.title 
#' @param grids 
#' @param linear.axes 
#' @param slope.axis 
#' @param title.option 
#' @param shape 
#' @param draw.line 
#' @param ylab 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' probpaper("Weibull",
#'           xlim = c(1, 10), 
#'           grid = TRUE, 
#'           ylim = c(0.011,0.981))
#' 
#' probpaper("Weibull",
#'           xlim = c(1, 100),
#'           grid = TRUE,
#'           ylim = c(0.011,0.981))
#' 
#' probpaper("Weibull",
#'           xlim = c(1, 1000),
#'           grid = TRUE,
#'           ylim = c(0.011,0.981))
#' 
#' probpaper("Weibull",
#'           xlim = c(1, 1000),
#'           grid = TRUE, 
#'           ylim = c(0.0011,0.9981))
#' 
#' probpaper("Lognormal",
#'           xlim = c(1, 10),
#'           grid = TRUE,
#'           ylim = c(0.011,0.981))
#' 
#' probpaper("Lognormal",
#'           xlim = c(1, 100),
#'           grid = TRUE,
#'           ylim = c(0.011,0.981))
#' 
#' probpaper("Lognormal",
#'           xlim = c(1, 1000),
#'           grid = TRUE,
#'           ylim = c(0.011,0.981))
#' }
probpaper <-
function (distribution, xlab = "Time", xlim = c(1, 10), ylim = c(0.011, 
    0.99), original.par = F, my.title = NULL, cex = 1, cex.labs = 1.1, 
    cex.tic.lab = 1, cex.title = cex.labs, sub.title = NULL, 
    grids = FALSE, linear.axes = F, slope.axis = F, title.option = "blank", 
    shape = NULL, draw.line = F, ylab = GetSMRDDefault("SMRD.LabelOnYaxis")) 
{
    old.par <- par()
    par(mar = c(4, 5, 1, 2) + 0.1, err = -1)
    if (original.par) 
        on.exit(par(old.par))
    if (is.null(my.title)) {
        my.title <- ""
    }
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- c(1, 10)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- c(0.011, 0.99)[yrna]
    log.of.data <- probplot.setup(distribution, xlim, ylim, 
        my.title = my.title, sub.title = "", cex = cex, ylab = ylab, 
        xlab = "", grids = grids, linear.axes = linear.axes, 
        title.option = title.option, draw.line = draw.line, slope.axis = slope.axis, 
        shape = shape, cex.title = cex.title, cex.labs = cex.labs)
    title(xlab = xlab, cex = cex.labs)
}
