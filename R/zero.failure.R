#' Determine test length for a zero-failure test as a multiple of spec
#'
#' @param beta 
#' @param quantile 
#' @param conlev 
#' @param n 
#' @param printem 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' zero.failure.k(beta = 2, quantile = 0.1, conlev = 0.99,	n = 5)
#' 
#' zero.failure.k(beta = 1, quantile = 0.01, conlev = 0.95, n = 5)
#' 
#' zero.failure.k(beta = 2, quantile = 0.01, conlev = 0.95, n = 5)
#' }
zero.failure.k <-
function (beta = 2, quantile = 0.01, conlev = 0.9, n = 10, printem = T) 
{
    k <- ((1/n) * (logb(1 - conlev)/logb(1 - quantile)))^(1/beta)
    if (printem) {
        cat("\nMinimum sample size demonsration test plan should run \n")
        cat("for", format(k), "times the", format(quantile), 
            "quantile specification. \n \n")
    }
    invisible(k)
}




#' Determine sample size for a zero-failure test
#'
#' @param conlev 
#' @param quantile 
#' @param k 
#' @param beta 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' zero.failure.n(conlev = 0.95, quantile = 0.01, k = 14, beta = 1)
#' 
#' zero.failure.n(conlev = 0.95, quantile = 0.01, k = 3.369, beta = 2)
#' 
#' }
zero.failure.n <-
  function (conlev, quantile, k, beta) 
  {
    n <- (logb(1 - conlev)/logb(1 - quantile))/exp(logb(k) * beta)
    
    n
  }



#' Number of test time units needed for minimum-sized demonstration
#'
#' @param betavec 
#' @param quantile 
#' @param conlev 
#' @param krange 
#' @param xlim 
#' @param ylim 
#' @param grid 
#' @param bw 
#' @param my.title 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' zero.failure.plan(xlim = c(1.51,3.99), 
#'                   ylim = c(.1,29), 
#'                   krange = c(1.5,3.83))
#' 
#' zero.failure.plan(betavec = c( 1., 2.), 
#'                   quantile = 0.01, 
#'                   conlev = 0.95, 
#'                   xlim = c(1.51,10), 
#'                   ylim = c(.1,199), 
#'                   krange = c(1.5,10),
#'                   grid = T,
#'                   bw = FALSE)
#' }
zero.failure.plan <-
  function (betavec = c(0.8, 1, 1.5, 2, 3), 
            quantile = 0.1, 
            conlev = 0.99,
            krange = c(1.5, 4), 
            xlim = NULL, 
            ylim = NULL, 
            grid = F,
            bw = FALSE, 
            my.title = NULL)
  {
    lwd.fix <- 2
    number.lines <- length(betavec)
    
    if(bw) {
      
       color.map <- rep(1, number.lines)
       ltyvec <- rep(c(1, 3, 4, 5, 6), number.lines)
       
     } else {
       
       color.map <- 1:number.lines
       ltyvec <- rep(1, number.lines)
       
     }
    
    old.par <- par(mar = c(5.1, 5.1, 4.1, 2.1))
    on.exit({ par(old.par) })
    
    if (is.null(my.title)) my.title <- paste("Zero-failure", 
                                             paste(floor(conlev * 100 + 0.01), "%", sep = "", collapse = ""), 
                                             "Demonstration Test that Reliability is at Least",
                                             format(1 - quantile))
    
    kvec <- seq(krange[1], krange[2], length = 25)
    nmat <- matrix(NA, nrow = length(kvec), ncol = length(betavec))
    
    for(i in 1:length(kvec)) {
      
        nmat[i, ] <- (logb(1 - conlev)/logb(1 - quantile))/exp(logb(kvec[i]) * betavec)
    }
    if (is.null(xlim)) xlim <- range(kvec)
    if (is.null(ylim)) ylim <- range(nmat)
    plot.paper(xlim, ylim, xlab = "", ylab = "", grids = grid)
    title(xlab = "Test Length as a Factor of Life-Length Specification",
          cex = 1.5)
    title(ylab = "Number of Units Tested with Zero Failures",
          cex = 1.5)
    title(main = my.title)
    for(i in 1:number.lines) {
      
        lines(kvec, 
              nmat[, i], 
              lty = ltyvec[i], 
              lwd = lwd.fix,
              col = color.map[i])
      
    }
    
    bty = "o"
    bg0 = 16
    legend(x.loc(0.75), 
           y.loc(0.75), 
           bty = bty, 
           bg = bg0, 
           paste("beta =", betavec), 
           cex = 1.1, 
           col = color.map[1:number.lines],
           lty = ltyvec[1:number.lines], 
           lwd = lwd.fix, 
           y.intersp = 0.675)
    
  }


#' Title
#'
#' @param alpha.vec 
#' @param quantile 
#' @param pfactor 
#' @param number.points 
#' @param grids 
#' @param bw 
#' @param my.title 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' zero.failure.prsd(alpha.vec = c(0.05,0.1), quantile = 0.01, pfactor = 3)
#'
#' }
zero.failure.prsd <-
  function (alpha.vec, quantile, pfactor = 3, number.points = 100,
            grids = TRUE, bw = FALSE, my.title = NULL)
  {
    quantile.low <- quantile^pfactor
    conf.level <- floor(100 * (1 - alpha.vec) + 0.01)
    rel.zero <- 1 - quantile
    rel.high <- 1 - quantile.low
    number.lines <- length(alpha.vec)
    lwd.fix <- 2
    if (bw) {
      color.map <- rep(1, number.lines)
      ltyvec <- rep(c(1, 3, 4, 5, 6), number.lines)
    }   else {
      color.map <- 1:number.lines
      ltyvec <- rep(1, number.lines)
    }
    old.par <- par(mar = c(5.1, 4.1, 4.1, 2.1))
    on.exit({
      par(old.par)
    })
    x.vec <- seq(rel.zero, rel.high, length = number.points)
    prsd.mat <- matrix(NA, ncol = number.lines, nrow = number.points)
    for (i in 1:number.lines) {
      prsd.mat[, i] <- x.vec^(log(alpha.vec[i])/log(rel.zero))
    }
    plot.paper(range(x.vec), range(prsd.mat), grids = grids,
               cex.tic.lab = 1.2)
    for (i in 1:number.lines) {
      lines(x.vec, prsd.mat[, i], lty = ltyvec[i], lwd = lwd.fix,
            col = color.map[i])
    }
    if (is.null(my.title))
      my.title <- paste("Probability of Successful Demonstration of Reliability",
                        rel.zero)
    title(main = my.title, cex.main = 1.2)
    title(xlab = "Actual Reliability", cex.lab = 1.2)
    title(ylab = "Probability 0 Failures", cex.lab = 1.2)
    bty = "o"
    bg0 = 16
    legend(x.loc(0.02), y.loc(0.95), bty = bty, bg = bg0, paste("Confidence =",
                                                                conf.level), cex = 1.1, col = color.map[1:number.lines],
           lty = ltyvec[1:number.lines], lwd = lwd.fix, y.intersp = 0.675)
  }
