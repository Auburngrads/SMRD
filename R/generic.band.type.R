generic.band.type <-
function (band.type, allow = F) 
{
    if (!is.character(band.type)) 
        stop(paste("Band.type must be character string:", band.type))
  
    switch(casefold(band.type), 
           n = , none = , None = { band.type <- "none" }, 
           p = , pointwise     = { band.type <- "pointwise" }, 
           s = , simultaneous  = { band.type <- "simultaneous" }, 
                                 {
           if (allow) {
                        return(NULL)
           } else {
                        warning(paste(band.type, 
                                      "is unrecognized band.type in generic.band.type(); assuming none"))
                                   band.type <- "none"}})
    return(band.type)
}
