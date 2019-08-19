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
#' gets.cdf.plot()
#' 
#' }
gets.cdf.plot <-
function (distvec = c("sev", "normal", "lev"),
          sigmavec = c(-.75, 0, .75),
          zetavec = c(0.5, 1, 2),
          alphavec = 0,
          len = 200,
          lwd = 1.5,
          cex = 16,
          plotem = T,...)
{
    
    distvec <- tolower(distvec)
    
    distvec <- gsub("^[pdqr]?sev[(_|\\s*)]?\\D*", "SEV", distvec)
    distvec <- gsub("^[pdqr]?lev[(_|\\s*)]?\\D*", "LEV", distvec)
    distvec <- gsub("^[pdqr]?nor[mal]?[(_|\\s*)]?\\D*", "Normal", distvec)
    
    distvec <- distvec[!duplicated(distvec)]
    
    if(length(distvec) > 3) stop("Incorrect distributions provided")
    
    sigmavec <- as.numeric(sigmavec)
    
    DIST  <- rep(distvec, each = len * length(sigmavec) * length(zetavec) * length(alphavec))
    SIG   <- rep(rep(sigmavec, each = len * length(zetavec) * length(alphavec)), by = length(distvec))
    ZETA  <- rep(rep(zetavec, each = len * length(alphavec)), by = length(sigmavec) * length(distvec))
    ALPHA <- rep(rep(alphavec, each = len), by = length(sigmavec) * length(distvec) * length(zetavec))
    
    DF <- data.frame(dist = DIST, 
                     sigma = SIG, 
                     zeta = ZETA,
                     alpha = ALPHA,
                     stringsAsFactors = F)

    lower <- matrix(rep(-4, length.out = length(distvec) * length(sigmavec)), 
                    byrow = T,
                    nrow = length(distvec),
                    ncol = length(sigmavec))
    
    upper <- matrix(rep(4, length.out = length(distvec) * length(sigmavec)), 
                    byrow = T,
                    nrow = length(distvec),
                    ncol = length(sigmavec))
    
    TVEC <- c()
    GETS <- c()
    
    for(j in 1:length(distvec)) {
        
        for(i in 1:length(sigmavec)) {
          
            for(k in 1:length(zetavec)) {
              
                for(l in 1:length(alphavec)) {
            
                    tvec <- seq(lower[j, i], upper[j, i], length = len)
                    TVEC <- c(TVEC,tvec)
                    gets <- pgets(tvec,alphavec[l],zetavec[k],sigma = sigmavec[i], distribution = distvec[j])
                    GETS <- c(GETS,gets)
                
                }
            
            }
        
        }
      
    }

DF$tvec = TVEC
DF$gets = GETS

DF$dist[DF[["dist"]] == "Normal"] <- "NORGETS"
DF$dist[DF[["dist"]] == "SEV"] <- "SEVGETS"
DF$dist[DF[["dist"]] == "LEV"] <- "LEVGETS"

RET <- list(norgets = subset(DF, "dist" == "NORGETS"),
            sevgets = subset(DF, "dist" == "SEVGETS"),
            levgets = subset(DF, "dist" == "LEVGETS"))

DF$params = paste("\u03B6",",","\u03B1"," = ",
                  DF$zeta,",", DF$alpha, sep = "")

DF$zeta  = as.factor(DF$zeta)
DF$alpha = as.factor(DF$alpha)

DF$sigma = paste("\u03C3 = ", DF$sigma, sep = "")

colnames(DF) <- c("Distribution","Sigma","Zeta","Alpha","Time","gets","Parameters")

if(!plotem) return(RET)
    
  p <- ggplot2::ggplot(DF) +
         ggplot2::geom_line(ggplot2::aes(x = Time,
                                         y = gets,
                                         group    = Parameters, 
                                         colour   = Parameters,
                                         linetype = Parameters),
                            size = lwd) +
         #theme(line = element_line(size = 20)) +
         ggplot2::theme_bw(base_size = cex) +
         ggplot2::facet_grid(Distribution ~ Sigma, scales = 'free') +
         ggplot2::xlab("Time (t)") +
         ggplot2::ylab("CDF - F(t|\u03B6,\u03C3,\u03B1)")
      
  invisible(RET)
  
  print(p)

}
