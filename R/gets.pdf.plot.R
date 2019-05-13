#' Title
#'
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' gets.pdf.plot()
#' 
#' }
gets.pdf.plot <-
function (...) 
{
    old.par <- par(err = -1, mfrow = c(3, 3), oma = c(0, 6, 2, 
        0))
    on.exit({
        par(old.par)
        par(new = FALSE)
    })
    distvec <- c("sev", "normal", "lev")
    sigmavecchar <- c("-.75", "0", ".75")
    sigmavec <- as.numeric(sigmavecchar)
    varzetavec <- c(0.5, 1, 2)
    labxy <- matrix(as.numeric(c("-0.56", "1.35", "-0.031", ".90", 
        ".4828", "1.352")), byrow = T, ncol = 2)
    xdgets <- function(tvec, varzeta, sigma, distribution) {
        dgets(tvec, 0, sigma, varzeta, distribution)
    }
    lower <- matrix(c(-3, -3, -2, -2, -4, -3, -3, -3, -2), byrow = T, 
        ncol = 3)
    upper <- matrix(c(2, 3, 3, 3, 4, 2, 2, 3, 3), byrow = T, 
        ncol = 3)
    for (j in 1:3) {
        for (i in 1:3) {
            tvec <- seq(lower[j, i], upper[j, i], length = 200)
            matplot(tvec, outer(tvec, varzetavec, xdgets, sigma = sigmavec[i], 
                distribution = distvec[j]), lty = 1:length(varzetavec), 
                type = "l", las = 1, cex = 1.2, xlab = "", ylab = "", 
                ...)
            if (j == 1) {
                if (is.postsctiptok()) {
                  slab <- paste("~f13~.s~f3~. = ", sigmavecchar[i], 
                    "~", sep = "")
                  mixed.text(labxy[i, 1], labxy[i, 2], slab, 
                    adj = 0.5, cex = 1.5)
                }
                else {
                  slab <- paste("s=", sigmavecchar[i], sep = "")
                  if (is.R()) 
                    text(labxy[i, 1], labxy[i, 2], slab, cex = 1.5, 
                      xpd = TRUE)
                  else text(labxy[i, 1], labxy[i, 2], slab, cex = 1.5)
                }
            }
        }
    }
    if (!is.R()) {
        cexylab <- 1.5
    }
    else {
        cexylab <- 1.2
    }
    mtext("LEV                        Normal                      SEV", 
        side = 2, outer = T, cex = cexylab, line = 1.5)
}
