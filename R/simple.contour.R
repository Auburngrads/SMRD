#' Create a contour plots of a likelihood function
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
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1, 
#'                                 censor.column = 3,
#'                                 time.units = "Kilometers")
#'
#' simple.contour(ShockAbsorber.ld,
#'                distribution = "lognormal",
#'                show.confidence = T)
#' 
#' 
#' simple.contour(ShockAbsorber.ld,
#'                distribution = "lognormal",
#'                show.confidence = T,
#'                threeD = T)
#'                
#' bulb.ld <- frame.to.ld(bulb,
#'                        response.column = 1,
#'                        data.title = "Bulb Data",
#'                        time.units = "Hours")
#' 
#' mlest(bulb.ld,"lognormal")
#' 
#' simple.contour(bulb.ld,"lognormal", quantile = .5)
#' 
#' # Bearing Cage Data Set
#' 
#' BearingCage.ld <- frame.to.ld(bearingcage,
#'                               response.column = 1, 
#'                               censor.column = 2, 
#'                               case.weight.column = 3,
#'                               time.units = "Hours")
#' 
#' simple.contour(BearingCage.ld,
#'                distribution = "Weibull",
#'                zoom.level = 4,
#'                quantile = .1,
#'                threeD = TRUE)
#' 
#' simple.contour(BearingCage.ld,
#'                distribution = "Weibull",
#'                zoom.level = 4,
#'                profile = "x")
#' 
#' simple.contour(BearingCage.ld,
#'                distribution = "Weibull",
#'                zoom.level = 4,
#'                profile = "y")
#' 
#' 
#' simple.contour(BearingCage.ld,
#'                distribution = "Weibull",
#'                threeD = T,
#'                zoom.level = 3,
#'                size = 75)
#' }
simple.contour <-
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
