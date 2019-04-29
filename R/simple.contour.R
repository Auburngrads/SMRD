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
