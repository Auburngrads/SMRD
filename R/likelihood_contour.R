#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param xlim 
#' @param ylim 
#' @param size 
#' @param quantile 
#' @param show.confidence 
#' @param contour.indicators 
#' @param zoom.level 
#' @param show.mle 
#' @param my.title 
#' @param threeD 
#' @param direction 
#' @param profile 
#' @param lwd 
#' @param elevation 
#' @param distance 
#' @param original.par 
#' @param log.quantile 
#' @param log.axis 
#' @param add 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @importFrom plotly layout plot_ly add_surface add_contour
likelihood_contour <- 
function (data.ld, 
          distribution, 
          xlim = c(NA, NA), 
          ylim = c(NA, NA), 
          size = 100, 
          quantile = NA, 
          show.confidence = TRUE, 
          contour.indicators = NULL, 
          zoom.level = 3.5, 
          show.mle = T, 
          my.title = NULL, 
          threeD = F, 
          direction = NULL, 
          profile = NULL, 
          lwd = 1,
          elevation = NULL, 
          distance = NULL, 
          original.par = T, 
          log.quantile = F,
          log.axis = T,
          add = F,...)
{
  
if(tolower(distribution)!='exponential') {
    
   rel.or.conf <- "Joint confidence region"
    
   if(!show.confidence) rel.or.conf <- 'Relative likelihood'
  
   likelihood.grid.out <- simple.grid(data.ld, 
                                      distribution,
                                      xlim = xlim, 
                                      ylim = ylim, 
                                      size = size, 
                                      the.quantile = quantile,
                                      factor = zoom.level, 
                                      log.quantile = log.quantile,...)
   
   p = plotly::plot_ly(z = likelihood.grid.out$z,
                       x = likelihood.grid.out$x, 
                       y = likelihood.grid.out$y, 
                       width = 800, 
                       height = 800)
   
   contours = list(z = list(show = TRUE,
                            usecolormap = TRUE,
                            highlightcolor = "#ff0000",
                            project = list(z = TRUE)))
   
   `if`(threeD,
        p <- plotly::add_surface(p, contours = contours),
        p <- plotly::add_contour(p, contours = contours))
   
   axs_titlefont <- list(family = "Arial, sans-serif",
                         size = 18,
                         color = "black")
   
   xaxs <- list(title = likelihood.grid.out$xlab,
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   yaxs <- list(title = likelihood.grid.out$ylab,
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   zaxs <- list(title = "Relative Likelihood",
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   `if`(threeD,
        p <- plotly::layout(p, scene = list(xaxis = xaxs, yaxis = yaxs, zaxis = zaxs)),
        p <- plotly::layout(p, xaxis = xaxs, yaxis = yaxs))

`if`(is.null(profile),
     plot.simple.contour(likelihood.grid.out, 
                         rel.or.conf = rel.or.conf,
                         contour.indicators = contour.indicators,
                         factor = zoom.level, 
                         show.mle = show.mle,
                         do.persp = threeD, 
                         direction = direction, 
                         elevation = elevation, 
                         distance = distance, 
                         original.par = original.par, 
                         add = add, 
                         lwd = lwd),
     profile.plot(profile.grid(likelihood.grid.out, 
                               which = as.character(profile))))

invisible(likelihood.grid.out)

} else {
    
    data.mle.exp <- expon.mle(data.ld)
    
    if(is.na(xlim[1]) | is.na(xlim[2])) stop('xlim values must be provided')
    
    data.mlprofile.exp <- one.dim.profile(data.mle.exp, 
                                          size = size * 2,
                                          range.list = list(log(xlim)),
                                          plot.em = F)
    
    profile.plot(transtruct(data.mlprofile.exp,"log"),log.axis = log.axis)
    
}

}
